use native_ts_parser::swc_core::common::{Span, Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::error::Error;
use crate::hir::{BinOp, PropertyDesc, UnaryOp, VarKind};
use crate::Symbol;
use crate::{
    common::VariableId,
    hir::{Callee, Expr, PropNameOrExpr, Stmt, Type},
    PropName,
};

use super::expr::MaybeTranslatedExpr;
use super::Transformer;
use super::ValueBinding;

mod do_while;
mod for_in_loop;
mod for_of_loop;

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
                    // no label found
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
            swc::Stmt::Decl(d) => {
                // transalte declare
                self.translate_decl(d)?;
            }
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
        // true if the label is shared with parent scope
        let mut old_break = false;
        // insert the label
        if let Some(label) = label {
            // insert label while checking if label already exist
            old_break = !self.break_labels.insert(label.to_string());
            // start of block
            self.context.func().stmts.push(Stmt::Block {
                label: label.to_string(),
            });
        }
        // open a new scope
        self.context.new_scope();
        // hoist the statements
        self.hoist_stmts(block.stmts.iter())?;

        // translate all the statements
        for stmt in &block.stmts {
            self.translate_stmt(stmt, None)?;
        }

        // close the scope
        self.context.end_scope();

        // remove the label
        if let Some(label) = label {
            // only remove if label is not shared
            if !old_break {
                self.break_labels.remove(label);
            }
            // end of block
            self.context.func().stmts.push(Stmt::EndBlock);
        }

        return Ok(());
    }

    pub fn translate_decl(&mut self, decl: &swc::Decl) -> Result<Option<Vec<VariableId>>> {
        match decl {
            // class declare
            swc::Decl::Class(c) => {
                let id = self.context.get_class_id(&c.ident.sym);
                self.translate_class(id, c.ident.sym.to_string(), &c.class)?;
                self.context.func().stmts.push(Stmt::DeclareClass(id));
            }
            // function declare
            swc::Decl::Fn(f) => {
                let id = self.context.get_func_id(&f.ident.sym);
                self.translate_function(id, None, &f.function)?;
                self.context.func().stmts.push(Stmt::DeclareFunction(id));
            }
            swc::Decl::TsEnum(_) => {
                // do nothing
            }
            swc::Decl::TsInterface(_) => {
                // do nothing
            }
            swc::Decl::TsModule(_) => {
                // do nothing
            }
            swc::Decl::TsTypeAlias(_) => {
                // do noting
            }
            // using variable declare
            swc::Decl::Using(u) => {
                let ids = self.translate_using_decl(u)?;
                return Ok(Some(ids));
            }
            // other variable declare
            swc::Decl::Var(decl) => {
                let ids = self.translate_var_decl(decl)?;
                return Ok(Some(ids));
            }
        }
        return Ok(None);
    }

    /// variable declaration
    pub fn translate_var_decl(&mut self, decl: &swc::VarDecl) -> Result<Vec<VariableId>> {
        // vec to store newly created ids
        let mut ids = Vec::new();

        // loop through each declorator
        for d in &decl.decls {
            let init = if let Some(e) = &d.init {
                Some(MaybeTranslatedExpr::NotTranslated(&e))
            } else {
                None
            };
            // translate the variable declare with pattern and initialiser
            ids.extend_from_slice(&self.translate_pat_var_decl(
                d.span,
                decl.kind.into(),
                &d.name,
                init,
                None,
            )?)
        }
        // return ids
        return Ok(ids);
    }

    fn translate_pat_var_decl(
        &mut self,
        span: Span,
        kind: VarKind,
        pat: &swc::Pat,
        init: Option<MaybeTranslatedExpr>,
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        match pat {
            // simple variable
            swc::Pat::Ident(id) => {
                Ok(vec![self.translate_ident_var_dec(
                    span, kind, id, init, parent_ann,
                )?])
            }
            // destructive array pattern
            swc::Pat::Array(a) => self.translate_array_pat_decl(kind, a, init, parent_ann),
            // destructive object pattern
            swc::Pat::Object(obj) => self.tranlslate_object_pat_decl(kind, obj, init, parent_ann),
            // assignment pattern, default initialiser
            swc::Pat::Assign(a) => {
                // not supported
                return Err(Error::syntax_error(
                    a.span,
                    "invalid left-hand side assignment",
                ));
            }
            // todo: rest assignment
            swc::Pat::Rest(r) => {
                return Err(Error::syntax_error(
                    r.dot3_token,
                    "rest assignment not supportd",
                ))
            }
            // exprssion pattern is only allowed in for-in loop, these are explicitly handled
            swc::Pat::Expr(_) | swc::Pat::Invalid(_) => {
                return Err(Error::syntax_error(
                    pat.span(),
                    "invalid left-hand side assignment",
                ))
            }
        }
    }

    fn translate_ident_var_dec(
        &mut self,
        span: Span,
        kind: VarKind,
        ident: &swc::BindingIdent,
        init: Option<MaybeTranslatedExpr>,
        parent_ann: Option<(Type, Span)>,
    ) -> Result<VariableId> {
        let varid = VariableId::new();

        let ty;
        let mut init_expr = None;

        let ty_ann = if let Some(ann) = &ident.type_ann {
            Some(self.translate_type(&ann.type_ann)?)
        } else {
            if let Some((ann, _)) = parent_ann {
                Some(ann)
            } else {
                None
            }
        };

        // 'const' declarations must be initialized.
        if kind == VarKind::Const && init.is_none() {
            return Err(Error::syntax_error(
                ident.span,
                "'const' declarations must be initialized.",
            ));
        }

        if let Some(init) = init {
            // translate the expression
            let (mut init, init_ty) = init.translate(self, ty_ann.as_ref())?;

            // type check init type can be assigned to annotated
            if let Some(ann) = &ty_ann {
                // type check
                self.type_check(ident.span(), &init_ty, ann)?;
                // variable type is its annotation
                ty = ty_ann;
            } else {
                // transform literal types
                match init_ty {
                    Type::LiteralBigint(_) => {
                        ty = Some(Type::Bigint);
                    }
                    Type::LiteralBool(_) => ty = Some(Type::Bool),
                    Type::LiteralInt(_) | Type::Int => {
                        ty = Some(Type::Number);
                        init = Expr::Cast {
                            span: DUMMY_SP,
                            value: Box::new(init),
                            ty: Type::Number,
                        };
                    }
                    Type::LiteralNumber(_) => ty = Some(Type::Number),
                    Type::LiteralString(_) => ty = Some(Type::String),
                    _ => ty = Some(init_ty),
                }
            }
            // set initial expression
            init_expr = Some(init);
        } else {
            // veriable type is its annotation
            ty = ty_ann;
        }

        // no initial expression or type annotation is found
        if ty.is_none() {
            return Err(Error::syntax_error(ident.span, "missing type annotation"));
        }

        let bind_success = match kind {
            VarKind::Using | VarKind::AwaitUsing => self.context.bind_using(
                &ident.sym,
                varid,
                ty.as_ref().unwrap().clone(),
                kind == VarKind::AwaitUsing,
            ),
            _ => self.context.bind_variable(
                &ident.sym,
                varid,
                ty.as_ref().unwrap().clone(),
                kind != VarKind::Const,
                kind == VarKind::Var,
            ),
        };

        // declare variable
        if !bind_success {
            if kind == VarKind::Var {
                if let Some(ValueBinding::Var {
                    redeclarable,
                    ty: var_ty,
                    ..
                }) = self.context.find_value_binding(&ident.sym)
                {
                    if *redeclarable {
                        if ty.as_ref().unwrap() != var_ty {
                            return Err(Error::syntax_error(ident.span, format!("Subsequent variable declarations must have the same type.  Variable '{}' must be of type '', but here has type ''.", ident.sym)));
                        }
                    }
                }
            }
            // the identifier is already declared
            return Err(Error::syntax_error(ident.span, "duplicated identifier"));
        }

        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: kind,
            id: varid,
            ty: ty.unwrap(),
        });

        if let Some(init) = init_expr {
            self.context
                .func()
                .stmts
                .push(Stmt::Expr(Box::new(Expr::VarAssign {
                    span: span,
                    op: crate::hir::AssignOp::Assign,
                    variable: varid,
                    value: Box::new(init),
                })));
        }

        return Ok(varid);
    }

    fn tranlslate_object_pat_decl(
        &mut self,
        kind: VarKind,
        obj: &swc::ObjectPat,
        init: Option<MaybeTranslatedExpr>,
        // annotation given by parent pattern
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        // id of the new variables
        let mut ids = Vec::new();

        // object pattern cannot be optional
        if obj.optional {
            return Err(Error::syntax_error(
                obj.span,
                "object pattern cannot be optional",
            ));
        }

        // translate type annotation
        let ty_ann = match &obj.type_ann {
            Some(ann) => {
                let ty = self.translate_type(&ann.type_ann)?;
                Some((ty, ann.span))
            }
            // stick to parent annotation
            None => parent_ann,
        };

        // translate initialiser
        let (init_expr, init_ty) = match init {
            Some(init) => init.translate(self, ty_ann.as_ref().map(|(a, _)| a))?,
            None => {
                return Err(Error::syntax_error(
                    obj.span,
                    "A destructuring declaration must have an initializer.",
                ))
            }
        };

        // push initialiser to stack
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::Push(Box::new(init_expr)))));

        // some if rest pattern reached
        let mut has_rest = None;
        let mut read_props = Vec::new();

        // loop through properties
        for prop in &obj.props {
            // rest is called
            if let Some(rest_span) = has_rest {
                return Err(Error::syntax_error(
                    rest_span,
                    "A rest element must be last in a destructuring pattern.",
                ));
            }

            // translate property
            match prop {
                swc::ObjectPatProp::KeyValue(kv) => {
                    // translate prperty name
                    let key = self.translate_prop_name(&kv.key)?;

                    // computed keys are not allowed
                    let key = match key{
                        PropNameOrExpr::PropName(p) => p,
                        PropNameOrExpr::Expr(_, _) => return Err(
                            Error::syntax_error(kv.key.span(), "A computed property name must be of type 'string', 'number' or 'symbol'")
                        )
                    };

                    // check type annotation
                    let parent_ann = match &ty_ann {
                        // get child annotation
                        Some((ann, span)) => match self.type_has_property(ann, &key, false) {
                            Some(ty) => Some((ty, *span)),
                            None => {
                                return Err(Error::syntax_error(
                                    kv.span(),
                                    format!("property '{}' does not exist on type ''", key),
                                ))
                            }
                        },
                        None => None,
                    };

                    // initialiser must have property
                    if let Some(prop_ty) = self.type_has_property(&init_ty, &key, false) {
                        let id = self.translate_pat_var_decl(
                            kv.span(),
                            kind,
                            &kv.value,
                            Some(MaybeTranslatedExpr::Translated(
                                Expr::Member {
                                    span: kv.span(),
                                    object: Box::new(Expr::ReadStack),
                                    key: PropNameOrExpr::PropName(key.clone()),
                                    optional: false,
                                },
                                prop_ty,
                            )),
                            parent_ann,
                        )?;

                        // push the new var ids
                        ids.extend_from_slice(&id);
                    } else {
                        return Err(Error::syntax_error(
                            kv.span(),
                            format!("property '{}' does not exist on type ''", key),
                        ));
                    };

                    // push the propname
                    read_props.push(key);
                }
                swc::ObjectPatProp::Assign(a) => {
                    let name = PropName::Ident(a.key.id.sym.to_string());

                    // translate type annotation
                    let ty_ann = match &ty_ann {
                        Some((ann, span)) => match self.type_has_property(ann, &name, false) {
                            Some(ann) => Some((ann, *span)),
                            None => {
                                return Err(Error::syntax_error(
                                    a.key.span,
                                    format!(
                                        "Property '{}' does not exist on type ''",
                                        a.key.id.sym
                                    ),
                                ))
                            }
                        },
                        None => None,
                    };

                    // translate default value
                    let default_value = match &a.value {
                        Some(expr) => {
                            let (default_expr, default_ty) =
                                self.translate_expr(&expr, ty_ann.as_ref().map(|(a, _)| a))?;
                            Some((default_expr, default_ty))
                        }
                        None => None,
                    };

                    match self.type_has_property(&init_ty, &name, false) {
                        Some(prop_ty) => {
                            // member expression
                            let mut member_expr = Expr::Member {
                                span: a.span,
                                object: Box::new(Expr::ReadStack),
                                key: PropNameOrExpr::PropName(name.clone()),
                                optional: default_value.is_some(),
                            };

                            if let Some((default_value, default_ty)) = default_value {
                                // get the variable type
                                let ty = if let Some((ty_ann, _)) = ty_ann {
                                    // type check
                                    self.type_check(
                                        a.span,
                                        &prop_ty,
                                        &ty_ann.clone().union(Type::Undefined),
                                    )?;
                                    ty_ann
                                } else {
                                    // default | property
                                    default_ty
                                        .union(self.remove_from_union(prop_ty, &Type::Undefined))
                                };

                                // select expression
                                let expr = Expr::Cast {
                                    span: a.span,
                                    value: Box::new(Expr::Bin {
                                        span: a.span,
                                        op: BinOp::Nullish,
                                        left: Box::new(member_expr),
                                        right: Box::new(default_value),
                                    }),
                                    ty: ty.clone(),
                                };

                                // translate var declare
                                let id = self.translate_ident_var_dec(
                                    a.span,
                                    kind,
                                    &a.key,
                                    Some(MaybeTranslatedExpr::Translated(expr, ty)),
                                    None,
                                )?;

                                // push new id to ids
                                ids.push(id);
                            } else {
                                // type check if annotated
                                let ty = if let Some((ty_ann, _)) = ty_ann {
                                    self.type_check(a.span, &prop_ty, &ty_ann)?;
                                    self.cast(&mut member_expr, &prop_ty, &ty_ann);
                                    ty_ann
                                } else {
                                    prop_ty
                                };

                                // simple declare
                                let id = self.translate_ident_var_dec(
                                    a.span,
                                    kind,
                                    &a.key,
                                    Some(MaybeTranslatedExpr::Translated(member_expr, ty)),
                                    None,
                                )?;

                                // push new id to ids
                                ids.push(id);
                            }
                        }
                        None => {
                            // initialiser does not have property, but a default is provided
                            if let Some((default_value, default_ty)) = default_value {
                                // declare variable
                                let id = self.translate_ident_var_dec(
                                    a.span,
                                    kind,
                                    &a.key,
                                    Some(MaybeTranslatedExpr::Translated(
                                        default_value,
                                        default_ty,
                                    )),
                                    ty_ann,
                                )?;

                                // push id to ids
                                ids.push(id);
                            } else {
                                // initialiser does not have poperty, no default value
                                return Err(Error::syntax_error(
                                    a.key.span,
                                    format!(
                                        "Property '{}' does not exist on type ''",
                                        a.key.id.sym
                                    ),
                                ));
                            }
                        }
                    }

                    // push name to read
                    read_props.push(name);
                }
                swc::ObjectPatProp::Rest(r) => {
                    // set has rest
                    has_rest = Some(r.span);

                    // annotation not allowed
                    if let Some(ann) = &r.type_ann {
                        return Err(Error::syntax_error(ann.span, "type annotation not allowed"));
                    }

                    let mut props = Vec::new();
                    let mut exprs = Vec::new();

                    // loop through all properties of initialiser
                    for p in self.get_properties(&init_ty) {
                        // property already read in destructive object
                        if read_props.contains(&p.name) {
                            continue;
                        }
                        // type annotation
                        let ty_ann = match &ty_ann {
                            Some((ann_ty, span)) => self
                                .type_has_property(ann_ty, &p.name, false)
                                .map(|t| (t, *span)),
                            None => None,
                        };

                        // member expression
                        let mut expr = Expr::Member {
                            span: r.span,
                            // read initialiser from stack
                            object: Box::new(Expr::ReadStack),
                            // read property from initialiser
                            key: PropNameOrExpr::PropName(p.name.clone()),
                            // not optional
                            optional: false,
                        };

                        // get variable type
                        let ty = if let Some((ty_ann, ty_ann_span)) = ty_ann {
                            // property type must fulfill annotation
                            self.type_check(ty_ann_span, &p.ty, &ty_ann)?;
                            // cast type
                            self.cast(&mut expr, &p.ty, &ty_ann);
                            // use type annotation
                            ty_ann
                        } else {
                            // use property type
                            p.ty.clone()
                        };

                        // push object
                        exprs.push((p.name.clone(), expr));
                        props.push(PropertyDesc {
                            name: p.name.clone(),
                            ty: ty,
                            readonly: false,
                        });
                    }

                    // construct object expression and type
                    let obj_ty = Type::LiteralObject(props.into());
                    let obj_expr = Expr::Object {
                        span: r.span,
                        props: exprs,
                    };

                    // translate variable declare
                    let id = self.translate_pat_var_decl(
                        r.span,
                        kind,
                        &r.arg,
                        Some(MaybeTranslatedExpr::Translated(obj_expr, obj_ty)),
                        None,
                    )?;

                    // append new ids
                    ids.extend_from_slice(&id);
                }
            }
        }
        // pop initialiser from stack
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::Pop)));

        // return id
        return Ok(ids);
    }

    fn translate_array_pat_decl(
        &mut self,
        kind: VarKind,
        pat: &swc::ArrayPat,
        init: Option<MaybeTranslatedExpr>,
        // annotation given by parent pattern
        parent_ann: Option<(Type, Span)>,
    ) -> Result<Vec<VariableId>> {
        // variable ids declared
        let mut ids = Vec::new();

        // array pattern cannot be optional
        if pat.optional {
            return Err(Error::syntax_error(
                pat.span,
                "array pattern cannot be optional",
            ));
        }
        // translate type annotation
        let ty_ann = if let Some(ann) = pat.type_ann.as_ref() {
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
            Some((ty, ann.span))
        } else {
            // no type annotation, use parent annotation
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
                Some((ann, ann_span))
            } else {
                // no annotation
                None
            }
        };

        // push the initialiser to stack and get its type
        let init_ty = match init {
            Some(init) => {
                // translate initialiser
                let (init_expr, init_ty) = init.translate(self, ty_ann.as_ref().map(|(a, _)| a))?;
                // push the initialiser to stack
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::Push(Box::new(init_expr)))));
                // return type
                init_ty
            }
            None => {
                return Err(Error::syntax_error(
                    pat.span,
                    "A destructuring declaration must have an initializer.",
                ))
            }
        };

        for i in 0..pat.elems.len() {
            // get the element type at index
            let ann_prop_ty = match &ty_ann {
                // array
                Some((Type::Array(a), span)) => Some((a.as_ref().clone(), *span)),
                // tuple
                Some((Type::Tuple(t), span)) => Some((t[i].clone(), *span)),
                _ => None,
            };

            // if some, a variable is declared, otherwise, skip index
            if let Some(p) = &pat.elems[i] {
                // check if initialiser has index
                if let Some(prop_ty) =
                    self.type_has_property(&init_ty, &PropName::Int(i as _), false)
                {
                    // construct member expression
                    let member_expr = Expr::Member {
                        span: p.span(),
                        object: Box::new(Expr::ReadStack),
                        key: PropNameOrExpr::PropName(PropName::Int(i as _)),
                        optional: false,
                    };
                    // translate pat variable declare
                    let vs = self.translate_pat_var_decl(
                        p.span(),
                        kind,
                        p,
                        Some(MaybeTranslatedExpr::Translated(member_expr, prop_ty)),
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
                if !self
                    .context
                    .bind_using(&ident.sym, varid, init_ty.clone(), decl.is_await)
                {
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
                        span: decl.span,
                        op: crate::hir::AssignOp::Assign,
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

        // translate break condition first
        let (test, _ty) = self.translate_expr(&d.test, Some(&Type::Bool))?;

        // create scope
        self.context.new_scope();
        // enter loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
            update: None,
            end_check: Some(Box::new(test)),
        });

        // translate blody
        self.translate_stmt(&d.body, None)?;

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
        // an older break label exist
        let mut old_break = false;
        // an older continue lable exist
        let mut old_continue = false;

        // the loop is labeled
        if let Some(label) = label {
            // insert new label
            old_break = !self.break_labels.insert(label.to_string());
            old_continue = !self.continue_labels.insert(label.to_string());
        }
        // push loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
            update: None,
            end_check: None,
        });
        // open new scope
        self.context.new_scope();

        // break if test
        let (test, _ty) = self.translate_expr(&w.test, Some(&Type::Bool))?;

        // get the current function
        let func = self.context.func();

        // break if test value is false
        func.stmts.push(Stmt::If {
            test: Box::new(Expr::Unary {
                span: w.test.span(),
                op: UnaryOp::LogicalNot,
                value: Box::new(test),
            }),
        });
        func.stmts.push(Stmt::Break(label.map(|l| l.to_string())));
        func.stmts.push(Stmt::EndIf);

        // translate body
        self.translate_stmt(&w.body, None)?;

        // end scope must go before end loop
        self.context.end_scope();
        // end loop
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
        // initialiser
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

        // translate update expression
        let update = if let Some(update) = &f.update {
            let (expr, _ty) = self.translate_expr(&update, None)?;
            Some(Box::new(expr))
        } else {
            None
        };

        // enter loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|s| s.to_string()),
            update: update,
            end_check: None,
        });

        // create new scope for loop body
        self.context.new_scope();

        // break if false
        if let Some(test_expr) = &f.test {
            // translate break condition
            let (test, _ty) = self.translate_expr(&test_expr, Some(&Type::Bool))?;
            let func = self.context.func();
            // break if not test
            func.stmts.push(Stmt::If {
                test: Box::new(Expr::Unary {
                    span: test_expr.span(),
                    op: crate::hir::UnaryOp::LogicalNot,
                    value: Box::new(test),
                }),
            });
            func.stmts.push(Stmt::Break(None));
            func.stmts.push(Stmt::EndIf);
        }

        // translate body
        self.translate_stmt(&f.body, None)?;

        // close scope for loop body
        self.context.end_scope();
        // end loop
        self.context.func().stmts.push(Stmt::EndLoop);

        // close scope for for-head
        self.context.end_scope();

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
            crate::hir::VariableDesc {
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
                span: f.left.span(),
                op: crate::hir::AssignOp::Assign,
                variable: counter,
                value: Box::new(Expr::Int(0)),
            })));

        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::hir::VariableDesc {
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
                        span: f.right.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Member {
                            span: f.right.span(),
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
                    crate::hir::VariableDesc {
                        ty: iterator_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::hir::VariableDesc {
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
                        span: f.right.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Call {
                            span: f.right.span(),
                            callee: Box::new(Callee::Member {
                                span: f.right.span(),
                                object: iterable_expr,
                                prop: PropNameOrExpr::PropName(PropName::Symbol(
                                    crate::Symbol::Iterator,
                                )),
                                optional: false,
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));
            }
        };

        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
            update: None,
            end_check: None,
        });

        // the breaking condition
        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // break if counter == length
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Bin {
                        span: f.left.span(),
                        op: crate::hir::BinOp::Gteq,
                        left: Box::new(Expr::VarLoad {
                            span: f.left.span(),
                            variable: counter,
                        }),
                        right: Box::new(Expr::VarLoad {
                            span: f.left.span(),
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
                        span: f.left.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Call {
                            span: f.left.span(),
                            callee: Box::new(Callee::Member {
                                span: f.right.span(),
                                object: Expr::VarLoad {
                                    span: Span::default(),
                                    variable: iterator_var,
                                },
                                prop: PropNameOrExpr::PropName(PropName::Ident("next".to_string())),
                                optional: false,
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
                        span: f.left.span(),
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
            Expr::Cast {
                span: f.left.span(),
                value: Box::new(Expr::VarUpdate {
                    span: f.left.span(),
                    op: crate::hir::UpdateOp::SuffixAdd,
                    variable: counter,
                }),
                ty: Type::Number,
            },
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
            crate::hir::VariableDesc {
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
                span: f.left.span(),
                op: crate::hir::AssignOp::Assign,
                variable: counter,
                value: Box::new(Expr::Int(0)),
            })));

        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // register the variable
                self.context.func().variables.insert(
                    iterator_var,
                    crate::hir::VariableDesc {
                        ty: iterable_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::hir::VariableDesc {
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
                        span: f.left.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(iterable_expr),
                    })));
                // assign array.length to iterator result
                self.context
                    .func()
                    .stmts
                    .push(Stmt::Expr(Box::new(Expr::VarAssign {
                        span: f.left.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Member {
                            span: f.left.span(),
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
                    crate::hir::VariableDesc {
                        ty: iterator_ty.clone(),
                        is_heap: false,
                    },
                );
                self.context.func().variables.insert(
                    iterator_result_var,
                    crate::hir::VariableDesc {
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
                        span: f.left.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_var,
                        value: Box::new(Expr::Call {
                            span: f.left.span(),
                            callee: Box::new(Callee::Member {
                                span: f.right.span(),
                                object: iterable_expr,
                                prop: PropNameOrExpr::PropName(PropName::Symbol(
                                    crate::Symbol::Iterator,
                                )),
                                optional: false,
                            }),
                            args: Vec::new(),
                            optional: false,
                        }),
                    })));
            }
        };

        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.to_string()),
            update: None,
            end_check: None,
        });

        // the breaking condition
        match &iterable_ty {
            Type::Tuple(_) | Type::Array(_) => {
                // break if counter == length
                self.context.func().stmts.push(Stmt::If {
                    test: Box::new(Expr::Bin {
                        span: f.left.span(),
                        op: crate::hir::BinOp::EqEqEq,
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
                        span: f.left.span(),
                        op: crate::hir::AssignOp::Assign,
                        variable: iterator_result_var,
                        value: Box::new(Expr::Call {
                            span: f.left.span(),
                            callee: Box::new(Callee::Member {
                                span: f.right.span(),
                                object: Expr::VarLoad {
                                    span: Span::default(),
                                    variable: iterator_var,
                                },
                                prop: PropNameOrExpr::PropName(PropName::Ident("next".to_string())),
                                optional: false,
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
                        span: f.left.span(),
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
                        span: f.left.span(),
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_var,
                        }),
                        key: PropNameOrExpr::Expr(
                            Box::new(Expr::VarUpdate {
                                span: f.left.span(),
                                op: crate::hir::UpdateOp::SuffixAdd,
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
                        span: f.left.span(),
                        object: Box::new(Expr::VarLoad {
                            span: Span::default(),
                            variable: iterator_var,
                        }),
                        key: PropNameOrExpr::Expr(
                            Box::new(Expr::VarUpdate {
                                span: f.left.span(),
                                op: crate::hir::UpdateOp::SuffixAdd,
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
                        span: f.left.span(),
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
            swc::ForHead::Pat(b) if b.is_expr() => {
                let expr = b.as_expr().unwrap();

                todo!()
            }
            swc::ForHead::Pat(p) => {
                match p.as_ref() {
                    swc::Pat::Ident(ident) => {
                        // translate variable assignment
                        let (e, _ty) = self.translate_var_assign(
                            ident.span,
                            &ident.id,
                            swc::AssignOp::Assign,
                            expr,
                            ty,
                        )?;

                        // insert assignment expression
                        self.context.func().stmts.push(Stmt::Expr(Box::new(e)));
                    }
                    // already handled
                    swc::Pat::Expr(_) => unreachable!(),
                    swc::Pat::Array(a) => {
                        // translate array pattern assignment
                        let (e, _ty) = self.translate_array_pat_assign(a, expr, ty)?;

                        self.context.func().stmts.push(Stmt::Expr(Box::new(e)));
                    }
                    swc::Pat::Object(o) => {
                        // translate object pattern assignment
                        let (e, _ty) = self.translate_object_pat_assign(o, expr, ty)?;

                        self.context.func().stmts.push(Stmt::Expr(Box::new(e)));
                    }
                    _ => {
                        return Err(Error::syntax_error(
                            p.span(),
                            "invalid left-hand side assignment",
                        ))
                    }
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
                            expr = Expr::Cast {
                                span: u.span,
                                value: Box::new(expr),
                                ty: t.clone(),
                            };
                        }

                        var_ty = t;
                    } else {
                        var_ty = ty;
                    };

                    if !self
                        .context
                        .bind_using(&ident.sym, var_id, var_ty.clone(), u.is_await)
                    {
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
                            span: u.span,
                            op: crate::hir::AssignOp::Assign,
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
                let decl = &v.decls[0];
                let init = match &decl.init {
                    Some(e) => Some(MaybeTranslatedExpr::NotTranslated(&e)),
                    None => None,
                };

                self.translate_pat_var_decl(v.span, v.kind.into(), &decl.name, init, None)?;
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

                        if catch_ty != Type::Any {
                            return Err(Error::syntax_error(ann.span, "Catch clause variable type annotation must be 'any' or 'unknown' if specified."));
                        }
                    }
                    self.context
                        .bind_variable(&ident.sym, varid, Type::Any, false, false);
                } else {
                    return Err(Error::syntax_error(pat.span(), "expected ident"));
                }
            } else {
                self.context.func().variables.insert(
                    varid,
                    crate::hir::VariableDesc {
                        ty: Type::Any,
                        is_heap: false,
                    },
                );
            }

            // enter catch
            self.context.func().stmts.push(Stmt::Catch(varid));

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
