use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    untyped_hir::{
        self as uhir, ClassType, EnumType, InterfaceMethod, InterfaceType, Type, UnknownId,
    },
    VarId, VarKind,
};

use super::{Result, Translater};

impl Translater {
    pub fn translate_stmts(&mut self, stmts: &[swc::Stmt]) -> Result<()> {
        self.hoist(stmts.iter())?;

        for s in stmts {
            self.translate_stmt(s, None)?;
        }

        return Ok(());
    }
    pub fn translate_stmt(&mut self, stmt: &swc::Stmt, label: Option<String>) -> Result<()> {
        match stmt {
            swc::Stmt::Block(b) => {
                self.context.new_scope();

                if let Some(l) = &label {
                    self.context.push_label(l, false);
                    self.context.function().stmts.push(uhir::Stmt::Block {
                        span: b.span,
                        label: l.clone(),
                    })
                }

                self.translate_stmts(&b.stmts)?;

                if label.is_some() {
                    self.context.function().stmts.push(uhir::Stmt::EndBlock);
                }

                self.context.close_scope();
            }
            swc::Stmt::Break(b) => {
                if let Some(l) = &b.label {
                    if !self.context.has_label(&l.sym, false) {
                        return Err(Error::syntax_error(
                            b.span,
                            format!("undefined label '{}'", l.sym),
                        ));
                    }
                }
                self.context.function().stmts.push(uhir::Stmt::Break {
                    span: b.span,
                    label: b.label.as_ref().and_then(|l| Some(l.sym.to_string())),
                })
            }
            swc::Stmt::Continue(c) => {
                if let Some(l) = &c.label {
                    if !self.context.has_label(&l.sym, false) {
                        return Err(Error::syntax_error(
                            c.span,
                            format!("undefined label '{}'", l.sym),
                        ));
                    }
                }
                self.context.function().stmts.push(uhir::Stmt::Continue {
                    span: c.span,
                    label: c.label.as_ref().and_then(|l| Some(l.sym.to_string())),
                })
            }
            swc::Stmt::Debugger(d) => {
                return Err(Error::syntax_error(
                    d.span,
                    "debbugger statement not allowed",
                ))
            }
            swc::Stmt::Decl(d) => {
                return self.translate_decl(d);
            }
            swc::Stmt::DoWhile(d) => return self.translate_do_while(d, label),
            swc::Stmt::Empty(_) => {}
            swc::Stmt::Expr(e) => {}
            swc::Stmt::For(f) => return self.translate_for_loop(f, label),
            swc::Stmt::ForIn(f) => return self.translate_for_in_loop(f, label),
            swc::Stmt::ForOf(f) => return self.translate_for_of_loop(f, label),
            swc::Stmt::If(i) => return self.translate_if_stmt(i),
            swc::Stmt::Labeled(l) => {
                return self.translate_stmt(&l.body, Some(l.label.sym.to_string()))
            }
            swc::Stmt::Return(r) => {
                let value = if let Some(arg) = &r.arg {
                    self.translate_expr(arg)?
                } else {
                    uhir::Expr::Undefined
                };
                self.context.function().stmts.push(uhir::Stmt::Return {
                    span: r.span,
                    value: value,
                })
            }
            swc::Stmt::Switch(s) => {}
            swc::Stmt::Throw(t) => {}
            swc::Stmt::Try(t) => {}
            swc::Stmt::While(w) => return self.translate_while_loop(w, label),
            swc::Stmt::With(w) => {
                return Err(Error::syntax_error(w.span, "with statement is obsolete"))
            }
        };

        return Ok(());
    }

    pub fn translate_do_while(
        &mut self,
        d: &swc::DoWhileStmt,
        label: Option<String>,
    ) -> Result<()> {
        // translate the test value
        let test = self.translate_expr(&d.test)?;

        // open new scope
        self.context.new_scope();

        // push label if some
        if let Some(l) = &label {
            self.context.push_label(l, true);
        }

        // push loop statement
        self.context.function().stmts.push(uhir::Stmt::Loop {
            span: d.span,
            label: label,
        });

        // translate the loop body
        self.translate_stmt(&d.body, None)?;

        // break if test is true
        self.context.function().stmts.push(uhir::Stmt::If {
            span: d.test.span(),
            test: test,
        });
        // break
        self.context.function().stmts.push(uhir::Stmt::Break {
            span: d.test.span(),
            label: None,
        });
        self.context.function().stmts.push(uhir::Stmt::EndIf);

        // end of loop
        self.context.function().stmts.push(uhir::Stmt::EndLoop);

        // close scope
        self.context.close_scope();
        return Ok(());
    }

    pub fn translate_while_loop(
        &mut self,
        d: &swc::WhileStmt,
        label: Option<String>,
    ) -> Result<()> {
        // translate the continuation condition
        let test = self.translate_expr(&d.test)?;

        // opens new scope
        self.context.new_scope();

        // push label if some
        if let Some(l) = &label {
            self.context.push_label(l, true);
        }

        // enter loop
        self.context.function().stmts.push(uhir::Stmt::Loop {
            span: d.span,
            label: label,
        });

        // if not true, break
        self.context.function().stmts.push(uhir::Stmt::If {
            span: d.test.span(),
            // logical not
            test: uhir::Expr::UnaryOp {
                span: d.test.span(),
                op: uhir::UnaryOp::LogicalNot,
                value: test.into(),
            },
        });
        // break
        self.context.function().stmts.push(uhir::Stmt::Break {
            span: d.test.span(),
            label: None,
        });
        // end if
        self.context.function().stmts.push(uhir::Stmt::EndIf);

        // translate loop body
        self.translate_stmt(&d.body, None)?;

        // end of loop
        self.context.function().stmts.push(uhir::Stmt::EndLoop);

        // close scope
        self.context.close_scope();
        return Ok(());
    }

    pub fn translate_for_loop(&mut self, f: &swc::ForStmt, label: Option<String>) -> Result<()> {
        // opens a new scope
        self.context.new_scope();

        // push label if some
        if let Some(l) = &label {
            self.context.push_label(l, true);
        }

        // translate initialiser if some
        if let Some(init) = &f.init {
            match init {
                // expression is invalid unless it is an ident
                swc::VarDeclOrExpr::Expr(e) => {
                    if let Some(id) = e.as_ident() {
                        // assignment
                    }
                    return Err(Error::syntax_error(
                        e.span(),
                        "initiator in for loop must have a declaration",
                    ));
                }
                // declared with var, let or const
                swc::VarDeclOrExpr::VarDecl(d) => {
                    // translate variable declare
                    self.translate_var_decl(d)?;
                }
            }
        };

        // enter the loop
        self.context.function().stmts.push(uhir::Stmt::Loop {
            span: f.span,
            label: label,
        });

        // translate the continuation condition if some
        if let Some(test) = &f.test {
            // translate test value
            let test_expr = self.translate_expr(test)?;

            // break if false
            self.context.function().stmts.push(uhir::Stmt::If {
                span: test.span(),
                // logical not
                test: uhir::Expr::UnaryOp {
                    span: test.span(),
                    op: uhir::UnaryOp::LogicalNot,
                    value: Box::new(test_expr),
                },
            });
            // break
            self.context.function().stmts.push(uhir::Stmt::Break {
                span: test.span(),
                label: None,
            });
            // end if
            self.context.function().stmts.push(uhir::Stmt::EndIf);
        }

        // translate loop body
        self.translate_stmt(&f.body, None)?;

        // translate update if some
        if let Some(update) = &f.update {
            self.translate_expr(&update)?;
        }

        // end of loop
        self.context.function().stmts.push(uhir::Stmt::EndLoop);

        // close scope
        self.context.close_scope();

        return Ok(());
    }

    pub fn translate_for_in_loop(
        &mut self,
        f: &swc::ForInStmt,
        label: Option<String>,
    ) -> Result<()> {
        todo!()
    }

    pub fn translate_for_of_loop(
        &mut self,
        f: &swc::ForOfStmt,
        label: Option<String>,
    ) -> Result<()> {
        // open new scope
        self.context.new_scope();

        // push label if some
        if let Some(l) = &label {
            self.context.push_label(l, true);
        }

        // translate the iterable target
        let iterable = self.translate_expr(&f.right)?;

        // todo: for..await in
        if f.is_await {
            todo!()
        }

        // store the span to avoid recalculation
        let span = f.right.span();

        // call iterable[Symbol.iterator],
        // for the iteration protocol, see ECMA sec-iteration
        let iterator = uhir::Expr::Call {
            span: span,
            // member call
            callee: uhir::Callee::Member {
                span: span,
                // iterable object
                obj: Box::new(iterable),
                // Symbol.iterator
                prop: uhir::PropName::Symbol(uhir::Symbol::Iterator),
                is_optchain: false,
            },
            // povide no arguments
            is_optchain: false,
            type_args: Vec::new(),
            args: Vec::new(),
        };

        // a temporary variable
        let iterator_id = VarId::new();
        let iterator_next_ty = Type::Unknown(UnknownId::new());
        let iterator_ty = Type::Iterator(Box::new(iterator_next_ty));

        // store the iterator in a temporary variable
        self.context.function().stmts.push(uhir::Stmt::Declare {
            span: span,
            kind: VarKind::Const,
            name: "__builtin_iterator_".to_string(),
            id: iterator_id,
            ty: iterator_ty.clone(),
            init: Some(iterator),
        });

        // enter the loop
        self.context.function().stmts.push(uhir::Stmt::Loop {
            span: f.span,
            label: label,
        });

        // the expression for getting the next set of result
        let get_next = uhir::Expr::Call {
            span: span,
            // member call, iterator.next()
            callee: uhir::Callee::Member {
                span: span,
                // read iterator from the temporary variable
                obj: Box::new(uhir::Expr::ReadVar {
                    span: span,
                    name: "__builtin_iterator".to_string(),
                    id: iterator_id,
                    ty: iterator_ty.clone(),
                }),
                // an iterator must have next method
                prop: uhir::PropName::Ident("next".to_string()),
                is_optchain: false,
            },
            // provides no arguments
            is_optchain: false,
            type_args: Vec::new(),
            args: Vec::new(),
        };

        // temporary variable for storing the next set of result
        let iterator_next_id = VarId::new();
        let iterator_next_ty = iterator_next_ty;

        // store result of iterator.next() to a variable
        self.context.function().stmts.push(uhir::Stmt::Declare {
            span: span,
            kind: VarKind::Const,
            name: "__builtin_iterator_next_".into(),
            id: iterator_next_id,
            ty: iterator_next_ty,
            init: Some(get_next),
        });

        // check if iterator is done
        self.context.function().stmts.push(uhir::Stmt::If {
            span: span,
            test: uhir::Expr::Member {
                span: span,
                // read next set of result from variable
                obj: Box::new(uhir::Expr::ReadVar {
                    span: span,
                    name: "__builtin_iterator_next_".into(),
                    id: iterator_next_id,
                    ty: iterator_next_ty,
                }),
                // if result is done
                prop: uhir::PropName::Ident("done".into()),
                is_optchain: true,
            },
        });
        // break
        self.context.function().stmts.push(uhir::Stmt::Break {
            span: span,
            label: None,
        });
        // end if
        self.context.function().stmts.push(uhir::Stmt::EndIf);

        // get the next value from result set
        let get_next_value = uhir::Expr::Member {
            span: span,
            obj: Box::new(uhir::Expr::ReadVar {
                span: span,
                name: "__builtin_iterator_next_".into(),
                id: iterator_next_id,
                ty: iterator_next_ty,
            }),
            prop: uhir::PropName::Ident("value".into()),
            is_optchain: true,
        };

        // get the span, kind and binding from left hand side
        let (span, kind, id) = match &f.left {
            // a var, let or const declare
            swc::ForHead::VarDecl(v) => {
                // only one binding should be specified
                if v.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        v.span,
                        "only one variable binding is allowed",
                    ));
                }
                // only ident binding is allowed
                if let Some(id) = v.decls[0].name.as_ident() {
                    // return kind and id
                    (v.span, v.kind.into(), id)
                } else {
                    // not an ident, must be destructive pattern
                    return Err(Error::syntax_error(
                        v.span,
                        "destructive assignment not allowed",
                    ));
                }
            }
            // using declare
            swc::ForHead::UsingDecl(u) => {
                // translte kind
                let kind = if u.is_await {
                    VarKind::AwaitUsing
                } else {
                    VarKind::Using
                };
                // only one bindng is allowed
                if u.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        u.span,
                        "only one variable binding is allowed",
                    ));
                }
                // only ident is allowed
                if let Some(id) = u.decls[0].name.as_ident() {
                    (u.span, kind, id)
                } else {
                    // not an ident, must be destructive pattern
                    return Err(Error::syntax_error(
                        u.span,
                        "destructive assignment not allowed",
                    ));
                }
            }
            _ => todo!(),
        };

        // translate type annotation of variable
        let ty = if let Some(ann) = &id.type_ann {
            // translate type if provided
            self.translate_ty(&ann.type_ann)?
        } else {
            // unknown if not provided
            Type::Unknown(UnknownId::new())
        };

        // translate the variable declare
        self.translate_var_binding_declare(id.span, kind, &id.id.sym, ty, Some(get_next_value))?;

        // translate loop body
        self.translate_stmt(&f.body, None)?;

        // end loop
        self.context.function().stmts.push(uhir::Stmt::EndLoop);

        // close scope
        self.context.close_scope();

        return Ok(());
    }

    pub fn translate_if_stmt(&mut self, i: &swc::IfStmt) -> Result<()> {
        // open new scope
        self.context.new_scope();

        // translate condition value
        let test = self.translate_expr(&i.test)?;
        // if true
        self.context.function().stmts.push(uhir::Stmt::If {
            span: i.span,
            test: test,
        });

        // translate if body
        self.translate_stmt(&i.cons, None)?;

        // end if
        self.context.function().stmts.push(uhir::Stmt::EndIf);

        // close scope
        self.context.close_scope();

        // if else exist
        if let Some(alt) = &i.alt {
            self.context.new_scope();

            // else
            self.context.function().stmts.push(uhir::Stmt::Else);

            // traslate else body
            self.translate_stmt(&alt, None)?;

            // end else
            self.context.function().stmts.push(uhir::Stmt::EndElse);

            self.context.close_scope();
        }

        return Ok(());
    }

    pub fn translate_decl(&mut self, decl: &swc::Decl) -> Result<()> {
        match decl {
            swc::Decl::Class(c) => {
                self.translate_class_decl(c)?;
            }
            swc::Decl::Fn(f) => {
                self.translate_function_decl(f)?;
            }
            swc::Decl::TsEnum(e) => {
                self.translate_enum_decl(e)?;
            }
            swc::Decl::TsInterface(i) => {
                self.translate_interface_decl(i)?;
            }
            swc::Decl::TsModule(m) => {
                todo!()
            }
            swc::Decl::TsTypeAlias(a) => {
                self.translate_type_alias(a)?;
            }
            swc::Decl::Using(u) => {
                self.translate_using_decl(u)?;
            }
            swc::Decl::Var(v) => {
                self.translate_var_decl(v)?;
            }
        };

        return Ok(());
    }

    pub fn translate_class_decl(&mut self, class_decl: &swc::ClassDecl) -> Result<()> {
        let class_binding = self.context.find(&class_decl.ident.sym).unwrap();
        let class = match class_binding {
            Binding::Class(c) => c,
            _ => panic!(),
        };

        let class = unsafe {
            (class.as_ref() as *const ClassType as *mut ClassType)
                .as_mut()
                .unwrap()
        };

        // class should not be defined
        debug_assert!(!class.is_definite);

        let ty = self.translate_class(&class_decl.class)?;

        // copy the class over
        *class = ty;
        class.is_definite = true;
        class.name = class_decl.ident.sym.to_string();

        return Ok(());
    }

    pub fn translate_enum_decl(&mut self, e: &swc::TsEnumDecl) -> Result<()> {
        // binding must exist as it is prevoiuly hoisted
        let enum_binding = self.context.find(&e.id.sym).unwrap();
        let enum_ty = enum_binding.as_enum().unwrap();

        // enum_ty should not not be define yet
        debug_assert!(!enum_ty.is_definite);
        // name of enum should be equal
        debug_assert!(enum_ty.name == e.id.sym.as_ref());

        // unsafe: cast reference to mutable reference
        let enum_ty = unsafe {
            (enum_ty.as_ref() as *const EnumType as *mut EnumType)
                .as_mut()
                .unwrap()
        };

        // loop through each member
        for member in &e.members {
            // translate name of member
            let propname = match &member.id {
                swc::TsEnumMemberId::Ident(id) => uhir::PropName::Ident(id.sym.to_string()),
                swc::TsEnumMemberId::Str(s) => uhir::PropName::String(s.value.to_string()),
            };

            // add variant to type
            enum_ty.variants.push(uhir::EnumVariant { name: propname });

            // does not allow initialiser
            if member.init.is_some() {
                return Err(Error::syntax_error(
                    member.span,
                    "initialsers not allowed in enum",
                ));
            }
        }

        // set enum is definite
        enum_ty.is_definite = true;

        return Ok(());
    }

    pub fn translate_interface_decl(&mut self, iface: &swc::TsInterfaceDecl) -> Result<()> {
        // binding must exist as it is prevoiuly hoisted
        let interface_binding = self.context.find(&iface.id.sym).unwrap();
        let iface_ty = interface_binding.as_interface().unwrap();

        // iface type should not be defined yet
        debug_assert!(!iface_ty.is_definite);
        // iface name should be equal
        debug_assert!(iface_ty.name == iface.id.sym.as_ref());

        // unsafe: cast reference to mutable reference
        let iface_ty = unsafe {
            (iface_ty.as_ref() as *const InterfaceType as *mut InterfaceType)
                .as_mut()
                .unwrap()
        };

        // translate generic params
        // generics are translated first because super types may use it as arguments
        if let Some(params) = &iface.type_params {
            // open a new scope to store generic bindings
            self.context.new_scope();
            iface_ty.generics = self.translate_generic_params(&params)?;
        };

        // loop through extended type (super types)
        for sup in &iface.extends {
            // type arguments for super type
            let mut ty_args = Vec::new();

            // translate type arguments if super type
            if let Some(args) = &sup.type_args {
                for arg in &args.params {
                    ty_args.push(self.translate_ty(arg)?)
                }
            };

            // translate the super type
            let super_ty = self.translate_type_expr(&sup.expr, ty_args)?;

            // add super type to interface type
            iface_ty.extends.push(super_ty);
        }

        // translate elements of interface
        for elem in &iface.body.body {
            // a function cannot have properties
            // function types are interfaces already
            if elem.is_ts_call_signature_decl() {
                return Err(Error::syntax_error(
                    elem.span(),
                    "call signiture is not allowed, use function types instead",
                ));
            };
            // since classes are not object to us, constructor signature is not allowed
            if elem.is_ts_construct_signature_decl() {
                return Err(Error::syntax_error(
                    elem.span(), 
                    "constructor signature is not allowed, interfaces are relative to an object, not a class"
                ));
            };

            // getters not supported
            if let Some(getter) = elem.as_mut_ts_getter_signature() {
                return Err(Error::syntax_error(
                    getter.span,
                    "getters not allowed, use properties instead",
                ));
            }

            // setters not supported
            if let Some(setter) = elem.as_mut_ts_setter_signature() {
                return Err(Error::syntax_error(
                    setter.span,
                    "setters not allowed, use properties instead",
                ));
            }

            // a property
            if let Some(prop) = elem.as_mut_ts_property_signature() {
                // translate propname
                let propname =
                // propname not computed, can only be ident
                if !prop.computed && prop.key.is_ident(){
                    uhir::PropName::Ident(prop.key.as_ident().unwrap().sym.to_string())
                } else{
                    self.translate_computed_prop_name(&prop.key)?
                };

                // initialiser is not allowed
                if prop.init.is_some() {
                    return Err(Error::syntax_error(
                        prop.span,
                        "interface properties cannot have initialiser",
                    ));
                }
                if !prop.params.is_empty() {
                    return Err(Error::syntax_error(prop.span, "invalid interface property"));
                }

                // translate type of property
                let ty = if let Some(ty) = &prop.type_ann {
                    self.translate_ty(&ty.type_ann)?
                } else {
                    return Err(Error::syntax_error(prop.span, "missing type annotation"));
                };

                // add property to interface type
                iface_ty.props.push(uhir::InterfaceProperty {
                    name: propname,
                    optinal: prop.optional,
                    readonly: prop.readonly,
                    ty: ty,
                });
            };

            // a method
            if let Some(method) = elem.as_mut_ts_method_signature() {
                // translate propname
                let propname =
                // propname not computed, can only be ident
                if !method.computed && method.key.is_ident(){
                    uhir::PropName::Ident(method.key.as_ident().unwrap().sym.to_string())
                } else{
                    self.translate_computed_prop_name(&method.key)?
                };

                // type params are not supported
                if let Some(ty_params) = &method.type_params {
                    // todo: type params in interface
                    return Err(Error::syntax_error(
                        ty_params.span,
                        "type params not allowed in interface",
                    ));
                };

                // translate the function type
                let fn_ty = self.translate_fn_ty(
                    &method.params,
                    method.type_params.as_deref(),
                    method.type_ann.as_deref(),
                )?;

                // add method to the interface type
                iface_ty.methods.push(InterfaceMethod {
                    name: propname,
                    optional: method.optional,
                    ty: fn_ty,
                });
            }
        };

        // close scope that is opened
        if iface.type_params.is_some() {
            // close previously opened scope
            self.context.close_scope();
        };

        // definite type
        iface_ty.is_definite = true;

        return Ok(());
    }

    pub fn translate_function_decl(&mut self, f: &swc::FnDecl) -> Result<()> {
        // binding must exist as it is previously hoisted
        let func_binding = self.context.find(&f.ident.sym).unwrap();
        let func = func_binding.as_function().unwrap();

        // function should not be defined
        debug_assert!(!func.is_definite);
        // its type should not be defined
        debug_assert!(!func.ty.is_definite);

        // unsafe: cast reference to mutable reference
        let func = unsafe {
            (func.as_ref() as *const uhir::Function as *mut uhir::Function)
                .as_mut()
                .unwrap()
        };
        // unsafe: cast reference to mutable reference
        let func_ty = unsafe {
            (func.ty.as_ref() as *const uhir::FunctionType as *mut uhir::FunctionType)
                .as_mut()
                .unwrap()
        };

        // translate the function
        let translated_function = self.translate_function(&f.function)?;

        // copy function over
        func.is_arrow = false;
        func.is_async = f.function.is_async;
        func.is_generator = f.function.is_generator;
        func.params = translated_function.params;
        func.stmts = translated_function.stmts;
        func.variables = translated_function.variables;
        
        // copy function type over
        *func_ty = translated_function.ty.as_ref().clone();

        // set definite
        func_ty.is_definite = true;
        func.is_definite = true;

        return Ok(())
    }

    pub fn translate_type_alias(&mut self, alias: &swc::TsTypeAliasDecl) -> Result<()> {
        // binding must exist as it is previously hoisted
        let binding = self.context.find(&alias.id.sym).unwrap();
        let alias_ty = binding.as_alias().unwrap();

        // alias type should not be defined
        debug_assert!(!alias_ty.is_definite);

        // unsafe: cast reference to mutable reference
        let alias_ty = unsafe {
            (alias_ty.as_ref() as *const uhir::AliasType as *mut uhir::AliasType)
                .as_mut()
                .unwrap()
        };

        if let Some(params) = &alias.type_params{
            // open new scope for generics
            self.context.new_scope();
            alias_ty.generics = self.translate_generic_params(params)?;
        };

        alias_ty.base = self.translate_ty(&alias.type_ann)?;

        // close previously opened scope
        if alias.type_params.is_some(){
            self.context.close_scope();
        };

        alias_ty.is_definite = true;

        return Ok(())
    }

    pub fn translate_var_decl(&mut self, v: &swc::VarDecl) -> Result<()> {
        let kind: VarKind = v.kind.into();

        for decl in &v.decls {
            self.translate_var_declarator(kind, decl)?;
        }

        return Ok(());
    }

    pub fn translate_using_decl(&mut self, u: &swc::UsingDecl) -> Result<()> {
        let kind = if u.is_await {
            VarKind::AwaitUsing
        } else {
            VarKind::Using
        };

        for decl in &u.decls {
            self.translate_var_declarator(kind, decl)?;
        }

        return Ok(());
    }

    pub fn translate_var_declarator(
        &mut self,
        kind: VarKind,
        decl: &swc::VarDeclarator,
    ) -> Result<()> {
        if decl.init.is_none() {
            return Err(Error::syntax_error(decl.span, "variable missing initiator"));
        };

        let init = self.translate_expr(&decl.init.as_ref().unwrap())?;

        self.translate_pat_decl(kind, &decl.name, init)?;

        return Ok(());
    }

    pub fn translate_pat_decl(
        &mut self,
        kind: VarKind,
        pat: &swc::Pat,
        init: uhir::Expr,
    ) -> Result<()> {
        match pat {
            swc::Pat::Ident(id) => {
                let ty = if let Some(ann) = &id.type_ann {
                    self.translate_ty(&ann.type_ann)?
                } else {
                    Type::Unknown(UnknownId::new())
                };

                self.translate_var_binding_declare(id.span, kind, &id.id.sym, ty, Some(init))?;
            }
            _ => todo!(),
        }

        return Ok(());
    }

    pub fn translate_var_binding_declare(
        &mut self,
        span: Span,
        kind: VarKind,
        name: &str,
        ty: Type,
        initialiser: Option<uhir::Expr>,
    ) -> Result<()> {
        // should be hoisted
        let binding = self.context.find(name).unwrap();

        match binding {
            Binding::AwaitUsing { id, ty }
            | Binding::Using { id, ty }
            | Binding::Const { id, ty }
            | Binding::Let { id, ty }
            | Binding::Var { id, ty } => {
                self.context.function().stmts.push(uhir::Stmt::Declare {
                    span: span,
                    kind: kind,
                    name: name.to_string(),
                    id: *id,
                    ty: *ty,
                    init: initialiser,
                });
            }
            _ => panic!(),
        };

        return Ok(());
    }
}
