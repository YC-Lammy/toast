use native_ts_parser::swc_core::common::{Span, Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast::{UnaryOp, VarKind};
use crate::error::Error;
use crate::Symbol;
use crate::{
    ast::{Callee, Expr, PropNameOrExpr, Stmt, Type},
    common::VariableId,
    PropName,
};

use super::{context::Binding, Transformer};

type Result<T> = std::result::Result<T, Error>;

impl Transformer {
    /// translates a statement
    pub fn translate_stmt(&mut self, stmt: &swc::Stmt, label: Option<&str>) -> Result<()> {
        match stmt {
            swc::Stmt::Block(b) => self.translate_block_stmt(b, label)?,
            swc::Stmt::Break(b) => {
                // check for label
                if let Some(label) = &b.label {
                    if !self.break_labels.contains(label.sym.as_ref()) {
                        return Err(Error::syntax_error(label.span, "undefined label"));
                    }
                }
                // push break stmt
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Break(label.map(|l| l.to_string())));
            }
            swc::Stmt::Continue(c) => {
                // check for label
                if let Some(label) = &c.label {
                    if !self.continue_labels.contains(label.sym.as_ref()) {
                        return Err(Error::syntax_error(label.span, "undefined label"));
                    }
                }
                // push continue stmt
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Continue(label.map(|l| l.to_string())))
            }
            swc::Stmt::Debugger(_d) => {
                // todo: debugger
                return Ok(());
            }
            swc::Stmt::Decl(d) => self.translate_decl(d)?,
            swc::Stmt::DoWhile(d) => self.translate_do_while(d, label)?,
            swc::Stmt::Empty(_) => {
                // does nothing
            }
            swc::Stmt::Expr(e) => {
                // translate the expression
                let (expr, _ty) = self.translate_expr(&e.expr, None)?;
                self.context.func().stmts.push(Stmt::Expr(Box::new(expr)));
            }
            swc::Stmt::For(f) => self.translate_for_stmt(f, label)?,
            swc::Stmt::ForIn(f) => self.translate_for_in_stmt(f, label)?,
            swc::Stmt::ForOf(f) => self.translate_for_of_stmt(f, label)?,
            swc::Stmt::If(i) => self.translate_if_stmt(i)?,
            swc::Stmt::Labeled(l) => self.translate_stmt(&l.body, Some(&l.label.sym))?,
            swc::Stmt::Return(r) => {
                // translate argument with expected return type
                let arg = if let Some(e) = &r.arg {
                    self.translate_expr(e, Some(&self.return_ty.clone()))?.0
                } else {
                    // check if undefined is returnable
                    self.type_check(r.span, &Type::Undefined, &self.return_ty.clone())?;
                    Expr::Undefined
                };

                // return
                self.context.func().stmts.push(Stmt::Return(Box::new(arg)));
            }
            swc::Stmt::Switch(s) => self.translate_switch_stmt(s)?,
            swc::Stmt::Throw(t) => {
                // does not have to be type checked
                let (expr, _ty) = self.translate_expr(&t.arg, None)?;
                // push throw stmt
                self.context.func().stmts.push(Stmt::Throw(Box::new(expr)));
            }
            swc::Stmt::Try(t) => self.translate_try_catch_stmt(t)?,
            swc::Stmt::While(w) => self.translate_while_stmt(w, label)?,
            swc::Stmt::With(w) => {
                // with statement is deprecated
                return Err(Error::syntax_error(w.span, "with statement is deprecated"));
            }
        }

        return Ok(());
    }

    pub fn translate_block_stmt(
        &mut self,
        block: &swc::BlockStmt,
        label: Option<&str>,
    ) -> Result<()> {
        let mut old_break = false;
        // insert the label
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            // block
            self.context.func().stmts.push(Stmt::Block {
                label: label.to_string(),
            });
        }

        // open a new scope
        self.context.new_scope();

        self.hoist_stmts(block.stmts.iter())?;

        for stmt in &block.stmts {
            self.translate_stmt(stmt, None)?;
        }

        // close the scope
        self.context.end_scope();

        // remove the label
        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            // end block
            self.context.func().stmts.push(Stmt::EndBlock);
        }

        return Ok(());
    }

    pub fn translate_decl(&mut self, decl: &swc::Decl) -> Result<()> {
        match decl {
            swc::Decl::Class(c) => {
                let id = self.context.get_class_id(&c.ident.sym);
                self.translate_class(id, c.ident.sym.to_string(), &c.class)?;
                self.context.func().stmts.push(Stmt::DeclareClass(id));
            }
            swc::Decl::Fn(f) => {
                let id = self.context.get_func_id(&f.ident.sym);
                self.translate_function(id, None, &f.function)?;
                self.context.func().stmts.push(Stmt::DeclareFunction(id));
            }
            swc::Decl::TsEnum(_) => {
                // does nothing
            }
            swc::Decl::TsInterface(_) => {
                // does nothing
            }
            swc::Decl::TsModule(_) => {
                // does nothing
            }
            swc::Decl::TsTypeAlias(_) => {
                // does noting
            }
            swc::Decl::Using(u) => {
                self.translate_using_decl(u)?;
            }
            swc::Decl::Var(decl) => {
                self.translate_var_decl(decl)?;
            }
        }
        return Ok(());
    }

    /// variable declaration
    pub fn translate_var_decl(&mut self, decl: &swc::VarDecl) -> Result<Vec<VariableId>> {
        let mut ids = Vec::new();

        for d in &decl.decls {
            let init = if let Some(init) = &d.init {
                Some(self.translate_expr(&init, None)?)
            } else {
                None
            };

            ids.extend_from_slice(&self.translate_pat_var_decl(decl.kind, &d.name, init, None)?)
        }
        return Ok(ids);
    }

    fn translate_pat_var_decl(
        &mut self,
        kind: swc::VarDeclKind,
        pat: &swc::Pat,
        init: Option<(Expr, Type)>,
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        match pat {
            swc::Pat::Ident(id) => Ok(vec![
                self.translate_ident_var_dec(kind, id, init, parent_ann)?
            ]),
            swc::Pat::Array(a) => self.translate_array_pat_decl(kind, a, init, parent_ann),
            swc::Pat::Object(obj) => self.tranlslate_object_pat_decl(kind, obj, init, parent_ann),
            swc::Pat::Assign(a) => {
                return Err(Error::syntax_error(
                    a.span,
                    "invalid left-hand side assignment",
                ))
            }
            swc::Pat::Expr(e) => {
                return Err(Error::syntax_error(
                    e.span(),
                    "invalid left-hand side assignment",
                ))
            }
            swc::Pat::Invalid(i) => {
                return Err(Error::syntax_error(
                    i.span,
                    "invalid left-hand side assignment",
                ))
            }
            // todo: rest assignment
            swc::Pat::Rest(r) => {
                return Err(Error::syntax_error(
                    r.dot3_token,
                    "rest assignment not supportd",
                ))
            }
        }
    }

    fn translate_ident_var_dec(
        &mut self,
        kind: swc::VarDeclKind,
        ident: &swc::BindingIdent,
        init: Option<(Expr, Type)>,
        parent_ann: Option<(Type, Span)>,
    ) -> Result<VariableId> {
        let varid = VariableId::new();

        let mut ty = None;
        let mut init_expr = None;

        if let Some(ann) = &ident.type_ann {
            ty = Some(self.translate_type(&ann.type_ann)?);
        } else {
            if let Some((ann, _)) = parent_ann {
                ty = Some(ann);
            }
        }

        if let Some((mut init, init_ty)) = init {
            match init_ty {
                Type::LiteralBigint(_) => {
                    ty = Some(Type::Bigint);
                }
                Type::LiteralBool(_) => ty = Some(Type::Bool),
                Type::LiteralInt(_) | Type::Int => {
                    ty = Some(Type::Number);
                    init = Expr::Cast(Box::new(init), Type::Number);
                }
                Type::LiteralNumber(_) => ty = Some(Type::Number),
                Type::LiteralString(_) => ty = Some(Type::String),
                _ => ty = Some(init_ty),
            }

            init_expr = Some(init);
        }

        if ty.is_none() {
            return Err(Error::syntax_error(ident.span, "missing type annotation"));
        }

        // declare variable
        if !self.context.declare(
            &ident.sym,
            super::context::Binding::Var {
                writable: kind != swc::VarDeclKind::Const,
                redeclarable: kind == swc::VarDeclKind::Var,
                id: varid,
                ty: ty.as_ref().unwrap().clone(),
            },
        ) {
            // the identifier is already declared
            return Err(Error::syntax_error(ident.span, "duplicated identifier"));
        }

        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: match kind {
                swc::VarDeclKind::Const => VarKind::Const,
                swc::VarDeclKind::Let => VarKind::Let,
                swc::VarDeclKind::Var => VarKind::Var,
            },
            id: varid,
            ty: ty.unwrap(),
        });

        if let Some(init) = init_expr {
            self.context
                .func()
                .stmts
                .push(Stmt::Expr(Box::new(Expr::VarAssign {
                    op: crate::ast::AssignOp::Assign,
                    variable: varid,
                    value: Box::new(init),
                })));
        }

        return Ok(varid);
    }

    fn tranlslate_object_pat_decl(
        &mut self,
        kind: swc::VarDeclKind,
        obj: &swc::ObjectPat,
        init: Option<(Expr, Type)>,
        // annotation given by parent pattern
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        if obj.optional {
            return Err(Error::syntax_error(
                obj.span,
                "object pattern cannot be optional",
            ));
        }

        todo!()
    }

    fn translate_array_pat_decl(
        &mut self,
        kind: swc::VarDeclKind,
        pat: &swc::ArrayPat,
        init: Option<(Expr, Type)>,
        // annotation given by parent pattern
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        // variable ids declared
        let mut ids = Vec::new();
        // todo: optional assignment
        if pat.optional {
            return Err(Error::syntax_error(
                pat.span,
                "array pattern cannot be optional",
            ));
        }
        // translate type annotation
        let (ty_ann, ty_ann_span) = if let Some(ann) = pat.type_ann.as_ref() {
            // type annotation is already given
            if parent_ann.is_some() {
                // a duplicated annotation happened
                return Err(Error::syntax_error(ann.span, "conflicted type annotation"));
            }
            // translate type annotation
            let ty = self.translate_type(&ann.type_ann)?;
            // only accepts array or tuple
            match &ty {
                Type::Array(_) => {
                    // do nothing
                }
                Type::Tuple(t) => {
                    // check tuple length
                    if t.len() != pat.elems.len() {
                        return Err(Error::syntax_error(
                            ann.span,
                            "number of elements in tuple does not match that of pattern",
                        ));
                    }
                }
                // not tuple or array
                _ => {
                    return Err(Error::syntax_error(
                        ann.span,
                        "expected array or tuple type",
                    ))
                }
            }
            // return type
            (Some(ty), Some(ann.span))
        } else {
            if let Some((ann, ann_span)) = parent_ann {
                // only accepts array or tuple
                match &ann {
                    // do nothing for array type
                    Type::Array(_) => {}
                    // tuple type must have exact length
                    Type::Tuple(t) => {
                        // check length of tuple type
                        if t.len() != pat.elems.len() {
                            return Err(Error::syntax_error(
                                ann_span,
                                "number of elements in tuple does not match that of pattern",
                            ));
                        }
                    }
                    // must be array or tuple
                    _ => {
                        return Err(Error::syntax_error(
                            ann_span,
                            "expected array or tuple type",
                        ))
                    }
                }
                // return annotation
                (Some(ann), Some(ann_span))
            } else {
                // no annotation
                (None, None)
            }
        };

        // push the initialiser to stack and get its type
        let init_ty = if let Some((e, t)) = init {
            // push the initialiser to stack
            self.context
                .func()
                .stmts
                .push(Stmt::Expr(Box::new(Expr::Push(Box::new(e)))));
            // return type
            Some(t)
        } else {
            // no initialiser
            None
        };

        for i in 0..pat.elems.len() {
            // get the type of element at index
            let ann_prop_ty = match &ty_ann {
                Some(Type::Array(a)) => Some((a.as_ref().clone(), ty_ann_span.unwrap())),
                Some(Type::Tuple(t)) => Some((t[i].clone(), ty_ann_span.unwrap())),
                _ => None,
            };

            // if some, a variable is declared, other wise, skip index
            if let Some(p) = &pat.elems[i] {
                // an initialiser is present
                if let Some(init_ty) = &init_ty {
                    // check if initialiser has index
                    if let Some(prop_ty) =
                        self.type_has_property(init_ty, &PropName::Int(i as _), false)
                    {
                        // construct member expression
                        let member_expr = Expr::Member {
                            object: Box::new(Expr::ReadStack),
                            key: PropNameOrExpr::PropName(PropName::Int(i as _)),
                            optional: false,
                        };
                        // translate pat variable declare
                        let vs = self.translate_pat_var_decl(
                            kind,
                            p,
                            Some((member_expr, prop_ty)),
                            ann_prop_ty,
                        )?;
                        // push ids
                        ids.extend_from_slice(&vs);
                    } else {
                        // the initialiser does not have index
                        return Err(Error::syntax_error(
                            p.span(),
                            format!("type '' has no property '{}'", i),
                        ));
                    }
                } else {
                    // no initialiser
                    let vs = self.translate_pat_var_decl(kind, p, None, ann_prop_ty)?;
                    // push ids
                    ids.extend_from_slice(&vs);
                }
            };
        }

        // pop the initialiser from stack
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::Pop)));

        return Ok(ids);
    }

    pub fn translate_using_decl(&mut self, decl: &swc::UsingDecl) -> Result<Vec<VariableId>> {
        let mut ids = Vec::new();

        for d in &decl.decls {
            if let Some(ident) = d.name.as_ident() {
                let varid = VariableId::new();
                ids.push(varid);

                let mut var_ty = None;

                if let Some(ann) = &ident.type_ann {
                    let ty = self.translate_type(&ann.type_ann)?;
                    var_ty = Some(ty);
                }
                let (init_expr, init_ty) = if let Some(init) = &d.init {
                    self.translate_expr(&init, var_ty.as_ref())?
                } else {
                    return Err(Error::syntax_error(d.span, "missing initialiser"));
                };

                let sym = if decl.is_await {
                    Symbol::AsyncDispose
                } else {
                    Symbol::Dispose
                };

                // type check
                if let Some(f) = self.type_has_property(&init_ty, &PropName::Symbol(sym), true) {
                    match f {
                        Type::Function(f) => {
                            if let Err(_) = self.type_check(DUMMY_SP, &init_ty, &f.this_ty) {
                                return Err(Error::syntax_error(
                                    d.span,
                                    format!(
                                        "'this' argument of '{}' is not compatable with type: ''",
                                        PropName::Symbol(sym)
                                    ),
                                ));
                            }
                            if !f.params.is_empty() {
                                return Err(Error::syntax_error(
                                    d.span,
                                    format!(
                                        "property '{}' expected function with 0 arguments",
                                        PropName::Symbol(sym)
                                    ),
                                ));
                            }
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                d.span,
                                format!(
                                    "property '{}' of type '' is not a functon",
                                    PropName::Symbol(sym)
                                ),
                            ))
                        }
                    }
                } else {
                    return Err(Error::syntax_error(
                        d.span,
                        format!("type '' has no property '{}'", PropName::Symbol(sym)),
                    ));
                }

                // declare variable
                if !self.context.declare(
                    &ident.sym,
                    super::context::Binding::Using {
                        is_await: decl.is_await,
                        id: varid,
                        ty: init_ty.clone(),
                    },
                ) {
                    return Err(Error::syntax_error(ident.span, "duplicated identifier"));
                }

                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: if decl.is_await {
                        VarKind::AwaitUsing
                    } else {
                        VarKind::Using
                    },
                    id: varid,
                    ty: init_ty.clone(),
                });

                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: varid,
                        value: Box::new(init_expr),
                    })));
            } else {
                return Err(Error::syntax_error(
                    d.span,
                    "destructive pattern not allowed",
                ));
            }
        }
        return Ok(ids);
    }

    pub fn translate_do_while(&mut self, d: &swc::DoWhileStmt, label: Option<&str>) -> Result<()> {
        let mut old_break = false;
        let mut old_continue = false;
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }

        self.context.new_scope();
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
        });

        // translate blody
        self.translate_stmt(&d.body, None)?;

        // break if test
        let (test, _ty) = self.translate_expr(&d.test, Some(&Type::Bool))?;

        let func = self.context.func();
        func.stmts.push(Stmt::If {
            test: Box::new(Expr::Unary {
                op: UnaryOp::LogicalNot,
                value: Box::new(test),
            }),
        });
        func.stmts.push(Stmt::Break(label.map(|l| l.to_string())));
        func.stmts.push(Stmt::EndIf);

        // end scope must go before end loop
        self.context.end_scope();
        // end loop
        self.context.func().stmts.push(Stmt::EndLoop);

        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            if !old_continue {
                self.continue_labels.remove(label);
            }
        }
        return Ok(());
    }

    pub fn translate_while_stmt(&mut self, w: &swc::WhileStmt, label: Option<&str>) -> Result<()> {
        let mut old_break = false;
        let mut old_continue = false;
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }
        // push loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
        });
        // open new scope
        self.context.new_scope();

        // break if test
        let (test, _ty) = self.translate_expr(&w.test, Some(&Type::Bool))?;

        let func = self.context.func();
        func.stmts.push(Stmt::If {
            test: Box::new(Expr::Unary {
                op: UnaryOp::LogicalNot,
                value: Box::new(test),
            }),
        });
        func.stmts.push(Stmt::Break(label.map(|l| l.to_string())));
        func.stmts.push(Stmt::EndIf);

        // end scope must go before end loop
        self.context.end_scope();

        // translate blody
        self.translate_stmt(&w.body, None)?;

        self.context.func().stmts.push(Stmt::EndLoop);

        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            if !old_continue {
                self.continue_labels.remove(label);
            }
        }

        return Ok(());
    }

    pub fn translate_for_stmt(&mut self, f: &swc::ForStmt, label: Option<&str>) -> Result<()> {
        // is a label with same name exist
        let mut old_break = false;
        let mut old_continue = false;
        // register label
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }

        // open new context
        self.context.new_scope();
        // initialise
        if let Some(init) = &f.init {
            match init {
                swc::VarDeclOrExpr::Expr(e) => {
                    // translate expression
                    let (e, _ty) = self.translate_expr(e, None)?;
                    // push expression
                    self.context.func().stmts.push(Stmt::Expr(Box::new(e)));
                }
                swc::VarDeclOrExpr::VarDecl(decl) => {
                    // only hoist non var
                    if decl.kind != swc::VarDeclKind::Var {
                        self.hoist_vardecl(decl)?;
                    }
                    // translate variable declare
                    self.translate_var_decl(decl)?;
                }
            }
        }
        // enter loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|s| s.to_string()),
        });

        // break if false
        if let Some(test) = &f.test {
            let (test, _ty) = self.translate_expr(&test, Some(&Type::Bool))?;
            let func = self.context.func();
            // break if not test
            func.stmts.push(Stmt::If {
                test: Box::new(Expr::Unary {
                    op: crate::ast::UnaryOp::LogicalNot,
                    value: Box::new(test),
                }),
            });
            func.stmts.push(Stmt::Break(None));
            func.stmts.push(Stmt::EndIf);
        }

        self.translate_stmt(&f.body, None)?;

        if let Some(update) = &f.update {
            let (expr, _ty) = self.translate_expr(&update, None)?;
            self.context.func().stmts.push(Stmt::Expr(Box::new(expr)));
        }

        self.context.end_scope();
        self.context.func().stmts.push(Stmt::EndLoop);

        // remove label
        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            if !old_continue {
                self.continue_labels.remove(label);
            }
        }
        return Ok(());
    }

    pub fn translate_for_in_stmt(&mut self, f: &swc::ForInStmt, label: Option<&str>) -> Result<()> {
        let mut old_break = false;
        let mut old_continue = false;
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }

        self.context.new_scope();

        let (iterable_expr, iterable_ty) = self.translate_expr(&f.right, None)?;

        // stores the iterator
        let iterator_var = VariableId::new();
        let iterator_result_var = VariableId::new();

        // counter stores index
        let counter = VariableId::new();

        // register counter variable
        self.context.func().variables.insert(
            counter,
            crate::ast::VariableDesc {
                ty: Type::Int,
                is_heap: false,
            },
        );
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Var,
            id: counter,
            ty: Type::Int,
        });
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                op: crate::ast::AssignOp::Assign,
                variable: counter,
                value: Box::new(Expr::Int(0)),
            })));

        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::ast::VariableDesc {
                        ty: Type::Int,
                        is_heap: false,
                    },
                );
                // declare variable
                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_var,
                    ty: Type::Int,
                });
                // assign array.length to iterator
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Member {
                            object: Box::new(iterable_expr),
                            key: PropNameOrExpr::PropName(PropName::Ident("length".to_string())),
                            optional: false,
                        }),
                    })));
            }
            _ => {
                let (iterator_ty, iterator_result_ty, _iterator_value_ty) =
                    self.type_is_iterable(f.right.span(), &iterable_ty)?;

                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::ast::VariableDesc {
                        ty: iterator_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::ast::VariableDesc {
                        ty: iterator_result_ty.clone(),
                        is_heap: false,
                    },
                );

                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_var,
                    ty: iterator_ty,
                });
                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_result_var,
                    ty: iterator_result_ty,
                });

                // iterator = iterable[Symbol.iterator]();
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Call {
                            callee: Box::new(Callee::Member {
                                object: iterable_expr,
                                prop: PropNameOrExpr::PropName(PropName::Symbol(
                                    crate::Symbol::Iterator,
                                )),
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));
            }
        };

        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
        });

        // the breaking condition
        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // break if counter == length
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Bin {
                        op: crate::ast::BinOp::Gteq,
                        left: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: counter,
                        }),
                        right: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_var,
                        }),
                    }),
                });
                self.context.func().stmts.push(Stmt::Break(None));
                self.context.func().stmts.push(Stmt::EndIf);
            }
            _ => {
                // iterator_result = iterator.next();
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Call {
                            callee: Box::new(Callee::Member {
                                object: Expr::VarLoad {
                                    span: Span::default(),
                                    variable: iterator_var,
                                },
                                prop: PropNameOrExpr::PropName(PropName::Ident("next".to_string())),
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));

                // if (iterator_result.done) {
                //    break
                // }
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Member {
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_result_var,
                        }),
                        key: PropNameOrExpr::PropName(PropName::Ident("done".to_string())),
                        optional: false,
                    }),
                });

                self.context.func().stmts.push(Stmt::Break(None));
                self.context.func().stmts.push(Stmt::EndIf);
            }
        };

        self.translate_for_head(
            &f.left,
            // counter++
            Expr::Cast(
                Box::new(Expr::VarUpdate {
                    op: crate::ast::UpdateOp::SuffixAdd,
                    variable: counter,
                }),
                Type::Number,
            ),
            Type::Number,
        )?;

        // translate the body
        self.translate_stmt(&f.body, None)?;

        // end scope
        self.context.end_scope();
        // end loop
        self.context.func().stmts.push(Stmt::EndLoop);

        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            if !old_continue {
                self.continue_labels.remove(label);
            }
        }
        return Ok(());
    }

    pub fn translate_for_of_stmt(&mut self, f: &swc::ForOfStmt, label: Option<&str>) -> Result<()> {
        let mut old_break = false;
        let mut old_continue = false;
        if let Some(label) = label {
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }

        self.context.new_scope();

        let (iterable_expr, iterable_ty) = self.translate_expr(&f.right, None)?;

        let mut iterator_value_ty = Type::Undefined;

        // stores the iterator
        let iterator_var = VariableId::new();
        let iterator_result_var = VariableId::new();

        // counter stores index
        let counter = VariableId::new();

        // register counter variable
        self.context.func().variables.insert(
            counter,
            crate::ast::VariableDesc {
                ty: Type::Int,
                is_heap: false,
            },
        );
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Var,
            id: counter,
            ty: Type::Int,
        });
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                op: crate::ast::AssignOp::Assign,
                variable: counter,
                value: Box::new(Expr::Int(0)),
            })));

        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::ast::VariableDesc {
                        ty: iterable_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::ast::VariableDesc {
                        ty: Type::Int,
                        is_heap: false,
                    },
                );
                // declare variable
                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_var,
                    ty: iterable_ty.clone(),
                });
                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_result_var,
                    ty: Type::Int,
                });
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(iterable_expr),
                    })));
                // assign array.length to iterator result
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Member {
                            object: Box::new(Expr::VarLoad {
                                span: Span::default(),
                                variable: iterator_var,
                            }),
                            key: PropNameOrExpr::PropName(PropName::Ident("length".to_string())),
                            optional: false,
                        }),
                    })));
            }
            _ => {
                let (iterator_ty, iterator_result_ty, value_ty) =
                    self.type_is_iterable(f.right.span(), &iterable_ty)?;

                iterator_value_ty = value_ty;

                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::ast::VariableDesc {
                        ty: iterator_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::ast::VariableDesc {
                        ty: iterator_result_ty.clone(),
                        is_heap: false,
                    },
                );

                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_var,
                    ty: iterator_ty,
                });
                self.context.func().stmts.push(Stmt::DeclareVar {
                    kind: VarKind::Var,
                    id: iterator_result_var,
                    ty: iterator_result_ty,
                });

                // iterator = iterable[Symbol.iterator]();
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Call {
                            callee: Box::new(Callee::Member {
                                object: iterable_expr,
                                prop: PropNameOrExpr::PropName(PropName::Symbol(
                                    crate::Symbol::Iterator,
                                )),
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));
            }
        };

        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
        });

        // the breaking condition
        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // break if counter == length
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Bin {
                        op: crate::ast::BinOp::EqEqEq,
                        left: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_result_var,
                        }),
                        right: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: counter,
                        }),
                    }),
                });
                self.context.func().stmts.push(Stmt::Break(None));
                self.context.func().stmts.push(Stmt::EndIf);
            }
            _ => {
                // iterator_result = iterator.next();
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        op: crate::ast::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Call {
                            callee: Box::new(Callee::Member {
                                object: Expr::VarLoad {
                                    span: Span::default(),
                                    variable: iterator_var,
                                },
                                prop: PropNameOrExpr::PropName(PropName::Ident("next".to_string())),
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));

                // if (iterator_result.done) {
                //    break
                // }
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Member {
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_result_var,
                        }),
                        key: PropNameOrExpr::PropName(PropName::Ident("done".to_string())),
                        optional: false,
                    }),
                });

                self.context.func().stmts.push(Stmt::Break(None));
                self.context.func().stmts.push(Stmt::EndIf);
            }
        };

        match &iterable_ty {
            Type::Array(elem) => {
                self.translate_for_head(
                    &f.left,
                    Expr::Member {
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_var,
                        }),
                        key: PropNameOrExpr::Expr(
                            Box::new(Expr::VarUpdate {
                                op: crate::ast::UpdateOp::SuffixAdd,
                                variable: counter,
                            }),
                            Type::Int,
                        ),
                        optional: false,
                    },
                    elem.as_ref().clone(),
                )?;
            }
            Type::Tuple(elems) => {
                self.translate_for_head(
                    &f.left,
                    Expr::Member {
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_var,
                        }),
                        key: PropNameOrExpr::Expr(
                            Box::new(Expr::VarUpdate {
                                op: crate::ast::UpdateOp::SuffixAdd,
                                variable: counter,
                            }),
                            Type::Int,
                        ),
                        optional: false,
                    },
                    Type::Union(elems.clone()),
                )?;
            }
            _ => {
                self.translate_for_head(
                    &f.left,
                    // counter++
                    Expr::Member {
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_result_var,
                        }),
                        key: PropNameOrExpr::PropName(PropName::Ident("value".to_string())),
                        optional: false,
                    },
                    iterator_value_ty,
                )?;
            }
        }

        // translate the body
        self.translate_stmt(&f.body, None)?;

        // end scope
        self.context.end_scope();
        // end loop
        self.context.func().stmts.push(Stmt::EndLoop);

        if let Some(label) = label {
            if !old_break {
                self.break_labels.remove(label);
            }
            if !old_continue {
                self.continue_labels.remove(label);
            }
        }
        return Ok(());
    }

    fn translate_for_head(
        &mut self,
        for_head: &swc::ForHead,
        mut expr: Expr,
        ty: Type,
    ) -> Result<()> {
        match for_head {
            swc::ForHead::Pat(p) => {
                if let Some(ident) = p.as_ident() {
                    if let Some(ann) = &ident.type_ann {
                        return Err(Error::syntax_error(ann.span, "type annotation not allowed"));
                    }

                    match self.context.find(&ident.sym).cloned() {
                        Some(Binding::Var {
                            writable,
                            id,
                            ty: var_ty,
                            ..
                        }) => {
                            self.type_check(ident.span, &ty, &var_ty)?;

                            if ty != var_ty {
                                expr = Expr::Cast(Box::new(expr), var_ty);
                            }

                            if !writable {
                                return Err(Error::syntax_error(
                                    ident.span,
                                    "cannot assign to constant variable",
                                ));
                            }
                            self.context
                                .func()
                                .stmts
                                .push(Stmt::Expr(Box::new(Expr::VarAssign {
                                    op: crate::ast::AssignOp::Assign,
                                    variable: id,
                                    value: Box::new(expr),
                                })));
                            return Ok(());
                        }
                        Some(Binding::Using { .. }) => {
                            return Err(Error::syntax_error(
                                ident.span,
                                "cannot assign to constant variable",
                            ))
                        }
                        None => {
                            return Err(Error::syntax_error(
                                ident.span,
                                format!("undefined identifier '{}'", ident.sym),
                            ))
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                ident.span,
                                format!(
                                    "expected variable, identifier '{}' is not a variable",
                                    ident.sym
                                ),
                            ))
                        }
                    }
                } else {
                    return Err(Error::syntax_error(
                        p.span(),
                        "for head can only have one variable binding",
                    ));
                }
            }
            swc::ForHead::UsingDecl(u) => {
                if u.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        u.span,
                        "for head can only have one variable binding",
                    ));
                }
                if let Some(ident) = u.decls[0].name.as_ident() {
                    let var_id = VariableId::new();
                    let var_ty;

                    if let Some(ann) = &ident.type_ann {
                        let t = self.translate_type(&ann.type_ann)?;
                        self.type_check(ann.span, &ty, &t)?;

                        if ty != t {
                            expr = Expr::Cast(Box::new(expr), t.clone());
                        }

                        var_ty = t;
                    } else {
                        var_ty = ty;
                    };

                    if !self.context.declare(
                        &ident.sym,
                        Binding::Using {
                            id: var_id,
                            ty: var_ty.clone(),
                            is_await: u.is_await,
                        },
                    ) {
                        return Err(Error::syntax_error(
                            ident.id.span,
                            format!("duplicated identifier '{}'", ident.sym),
                        ));
                    }

                    self.context.func().stmts.push(Stmt::DeclareVar {
                        kind: if u.is_await {
                            VarKind::AwaitUsing
                        } else {
                            VarKind::Using
                        },
                        id: var_id,
                        ty: var_ty,
                    });
                    self.context
                        .func()
                        .stmts
                        .push(Stmt::Expr(Box::new(Expr::VarAssign {
                            op: crate::ast::AssignOp::Assign,
                            variable: var_id,
                            value: Box::new(expr),
                        })));
                } else {
                    return Err(Error::syntax_error(
                        u.span,
                        "destructive pattern not allowed",
                    ));
                }
            }
            swc::ForHead::VarDecl(v) => {
                if v.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        v.span,
                        "for head can only have one variable binding",
                    ));
                }
                if let Some(ident) = v.decls[0].name.as_ident() {
                    let var_id = VariableId::new();
                    let var_ty;

                    if let Some(ann) = &ident.type_ann {
                        let t = self.translate_type(&ann.type_ann)?;
                        self.type_check(ann.span, &ty, &t)?;

                        if ty != t {
                            expr = Expr::Cast(Box::new(expr), t.clone());
                        }

                        var_ty = t;
                    } else {
                        var_ty = ty;
                    };

                    if !self.context.declare(
                        &ident.sym,
                        Binding::Var {
                            writable: v.kind != swc::VarDeclKind::Const,
                            redeclarable: v.kind == swc::VarDeclKind::Var,
                            id: var_id,
                            ty: var_ty.clone(),
                        },
                    ) {
                        return Err(Error::syntax_error(
                            ident.id.span,
                            format!("duplicated identifier '{}'", ident.sym),
                        ));
                    }

                    self.context.func().stmts.push(Stmt::DeclareVar {
                        kind: match v.kind {
                            swc::VarDeclKind::Const => VarKind::Const,
                            swc::VarDeclKind::Let => VarKind::Let,
                            swc::VarDeclKind::Var => VarKind::Var,
                        },
                        id: var_id,
                        ty: var_ty,
                    });
                    self.context
                        .func()
                        .stmts
                        .push(Stmt::Expr(Box::new(Expr::VarAssign {
                            op: crate::ast::AssignOp::Assign,
                            variable: var_id,
                            value: Box::new(expr),
                        })));
                } else {
                    return Err(Error::syntax_error(
                        v.span,
                        "destructive pattern not allowed",
                    ));
                }
            }
        }
        return Ok(());
    }

    pub fn translate_if_stmt(&mut self, i: &swc::IfStmt) -> Result<()> {
        self.context.new_scope();

        let (test, _ty) = self.translate_expr(&i.test, Some(&Type::Bool))?;
        self.context.func().stmts.push(Stmt::If {
            test: Box::new(test),
        });

        self.hoist_stmts([i.cons.as_ref()].into_iter())?;
        self.translate_stmt(&i.cons, None)?;

        self.context.end_scope();
        self.context.func().stmts.push(Stmt::EndIf);

        if let Some(alt) = &i.alt {
            self.context.new_scope();
            self.context.func().stmts.push(Stmt::Else);

            self.hoist_stmts([alt.as_ref()].into_iter())?;
            self.translate_stmt(&alt, None)?;

            self.context.end_scope();
            self.context.func().stmts.push(Stmt::EndElse);
        }
        return Ok(());
    }

    pub fn translate_try_catch_stmt(&mut self, t: &swc::TryStmt) -> Result<()> {
        self.context.func().stmts.push(Stmt::Try);

        self.translate_block_stmt(&t.block, None)?;

        self.context.func().stmts.push(Stmt::EndTry);

        if let Some(handler) = &t.handler {
            let varid = VariableId::new();

            self.context.new_scope();

            if let Some(pat) = &handler.param {
                // only accept ident
                if let Some(ident) = pat.as_ident() {
                    if let Some(ann) = &ident.type_ann {
                        let catch_ty = self.translate_type(&ann.type_ann)?;

                        if catch_ty != Type::Any{
                            return Err(Error::syntax_error(ann.span, "Catch clause variable type annotation must be 'any' or 'unknown' if specified."))
                        }
                    }
                    self.context.declare(
                        &ident.sym,
                        super::context::Binding::Var {
                            writable: false,
                            redeclarable: false,
                            id: varid,
                            ty: Type::Any,
                        },
                    );
                } else {
                    return Err(Error::syntax_error(pat.span(), "expected ident"));
                }
            } else {
                self.context.func().variables.insert(
                    varid,
                    crate::ast::VariableDesc {
                        ty: Type::Any,
                        is_heap: false,
                    },
                );
            }

            // enter catch
            self.context
                .func()
                .stmts
                .push(Stmt::Catch(varid));

            // translate the body
            self.translate_block_stmt(&handler.body, None)?;

            // must go before end catch
            self.context.end_scope();
            // end catch
            self.context.func().stmts.push(Stmt::EndCatch);
        }

        if let Some(finally) = &t.finalizer {
            self.context.func().stmts.push(Stmt::Finally);

            // translate the block
            self.translate_block_stmt(&finally, None)?;

            self.context.func().stmts.push(Stmt::EndFinally);
        }

        return Ok(());
    }

    pub fn translate_switch_stmt(&mut self, s: &swc::SwitchStmt) -> Result<()> {
        let (test, test_ty) = self.translate_expr(&s.discriminant, None)?;

        // push switch
        self.context.func().stmts.push(Stmt::Switch(Box::new(test)));

        for case in &s.cases {
            if let Some(expr) = &case.test {
                let (expr, _ty) = self.translate_expr(&expr, Some(&test_ty))?;

                self.context.new_scope();
                self.context
                    .func()
                    .stmts
                    .push(Stmt::SwitchCase(Box::new(expr)));

                self.hoist_stmts(case.cons.iter())?;

                for stmt in &case.cons {
                    self.translate_stmt(stmt, None)?;
                }

                self.context.end_scope();
                self.context.func().stmts.push(Stmt::EndSwitchCase);
            }
        }

        let mut default = false;
        for case in &s.cases {
            if case.test.is_none() {
                if default {
                    return Err(Error::syntax_error(
                        case.span,
                        "A 'default' clause cannot appear more than once in a 'switch' statement",
                    ));
                }
                default = true;
                self.context.func().stmts.push(Stmt::DefaultCase);
                self.context.new_scope();

                self.hoist_stmts(case.cons.iter())?;

                for stmt in &case.cons {
                    self.translate_stmt(stmt, None)?;
                }

                self.context.end_scope();
                self.context.func().stmts.push(Stmt::EndDefaultCase);
            }
        }

        // end switch
        self.context.func().stmts.push(Stmt::EndSwitch);

        return Ok(());
    }
}
