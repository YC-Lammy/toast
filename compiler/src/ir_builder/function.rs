use swc_common::BytePos;

use super::{context::Context, ir::VarArgId, *};

impl IRBuilder {
    pub fn translate_function(
        &mut self,
        container: &mut dyn IRContainer,
        func: &Function,
        funcid: FunctionId,
        in_class: Option<ClassID>,
    ) -> Result<FunctionId, Error> {
        if func.is_async && func.is_generator {
            return Err(Error::new(
                Span::new(
                    func.span_lo(),
                    BytePos(func.span_lo().0 + 7),
                    func.span.ctxt,
                ),
                "async generator function not supported",
                "",
            ));
        }
        let mut func_builder = IRFunctionBuilder {
            id: funcid,
            ir_module: container,

            is_arrow: false,
            is_async: func.is_async,
            is_generator: func.is_generator,
            is_constructor: false,
            class_function: in_class,
            captures: Vec::new(),
            heap_variables: Default::default(),
            own_variable: Default::default(),
            var_declares: HashMap::new(),
            bindings: Context::new(),
            context: Context::new(),
            ir: Vec::new(),
        };

        // find all the param names
        let mut param_names = Vec::new();
        for param in &func.params {
            let names = self.variable_names_from_pat(&param.pat);
            for name in names {
                func_builder
                    .declare_var(&name)
                    .expect("incorrect order of var declare");
                param_names.push((name, param.span));
            }
        }

        // check duplicated param names
        for i in 0..(param_names.len()) {
            let (name, _) = &param_names[i];

            for n in 0..param_names.len() {
                if i == n {
                    continue;
                }

                let (next_name, span) = &param_names[n];

                if next_name == name {
                    return Err(Error::new(
                        *span,
                        format!("Duplicate identifier '{}'.", name),
                        "",
                    ));
                }
            }
        }

        let mut i = 0;
        // translate assignment of params
        for param in &func.params {
            match &param.pat {
                // override rest assignment
                Pat::Rest(r) => {
                    if i + 1 != func.params.len() {
                        return Err(Error::new(
                            r.span,
                            "rest param must be the last param in function.",
                            "",
                        ));
                    }

                    // read the remaining params as array into ACC
                    func_builder.push(IR::ReadRemainParams { starting_from: i });

                    self.translate_assignment(&mut func_builder, &r.arg, Some(VarDeclKind::Var))?;
                }
                // use the normal assignment
                pattern => {
                    func_builder.push(IR::ReadParam(i));
                    self.translate_assignment(&mut func_builder, pattern, Some(VarDeclKind::Var))?;
                }
            }
            i += 1;
        }

        let func_body = func.body.as_ref().unwrap();

        for s in &func_body.stmts {
            self.find_var_declares(&mut func_builder, s);
        }

        self.hoist_declares(&mut func_builder, &func_body.stmts)?;

        for stmt in &func_body.stmts {
            self.translate_stmt(&mut func_builder, stmt, None)?;
        }

        let irfunc = func_builder.finish();

        self.functions.insert(funcid, irfunc);

        // initialise the function object
        container.push(IR::InitFunction(funcid));

        return Ok(funcid);
    }

    pub fn translate_arrow(
        &mut self,
        container: &mut dyn IRContainer,
        arrow: &ArrowExpr,
    ) -> Result<FunctionId, Error> {
        let funcid = FunctionId::new();

        let mut func_builder = IRFunctionBuilder {
            id: funcid,
            ir_module: container,

            is_arrow: true,
            is_async: arrow.is_async,
            is_generator: arrow.is_generator,
            is_constructor: false,
            class_function: None,
            captures: Vec::new(),
            heap_variables: Default::default(),
            own_variable: Default::default(),
            var_declares: HashMap::new(),
            bindings: Context::new(),
            context: Context::new(),
            ir: Vec::new(),
        };

        let mut i = 0;
        for p in &arrow.params {
            let names = self.variable_names_from_pat(p);
            for name in names {
                func_builder
                    .declare_var(&name)
                    .expect("incorrect order of var declare.");
            }
            func_builder.push(IR::ReadParam(i));
            self.translate_assignment(&mut func_builder, p, None)?;
            i += 1;
        }

        match arrow.body.as_ref() {
            BlockStmtOrExpr::BlockStmt(b) => {
                self.hoist_declares(&mut func_builder, &b.stmts)?;
                for s in &b.stmts {
                    self.translate_stmt(&mut func_builder, s, None)?;
                }
            }
            BlockStmtOrExpr::Expr(e) => {
                self.translate_expr(&mut func_builder, &e)?;
                func_builder.push(IR::Return);
            }
        };

        let irfunc = func_builder.finish();

        self.functions.insert(funcid, irfunc);

        container.push(IR::InitFunction(funcid));

        return Ok(funcid);
    }

    pub fn translate_call(
        &mut self,
        container: &mut dyn IRContainer,
        call: &CallExpr,
    ) -> Result<(), Error> {
        let mut arg_len = 0;
        let mut is_dynamic_call = false;
        let mut is_member_call = false;
        let mut is_static_call = None;
        let mut maybe_static_call = None;

        // check if any variable length arguments
        for arg in &call.args {
            if arg.spread.is_some() {
                is_dynamic_call = true;
            }
            arg_len += 1;
        }

        let args_list = ArgListId::new();
        let var_arg_id = VarArgId::new();

        if is_dynamic_call {
            container.push(IR::CreateVarArgList(var_arg_id));
        } else {
            container.push(IR::CreateArgList(args_list));
        }

        // translate the arguments
        for arg in &call.args {
            self.translate_expr(container, &arg.expr)?;

            if arg.spread.is_some() {
                let iterid = IterId::new();
                container.push(IR::CreateForOfIter(iterid));

                container.push(IR::Loop { label: None });
                container.push(IR::BreakIfIterDone(iterid));

                container.push(IR::IterNext(iterid));
                container.push(IR::PushVarArg(var_arg_id));

                container.push(IR::EndLoop);
                container.push(IR::DropIterator(iterid));
            } else if is_dynamic_call {
                container.push(IR::PushVarArg(var_arg_id));
            } else {
                container.push(IR::PushArg(args_list));
            }
        }

        match &call.callee {
            Callee::Super(s) => {
                if !container.is_constructor() {
                    return Err(Error::new(
                        s.span,
                        "super call is only allowed within constructors.",
                        "",
                    ));
                };

                container.push(IR::ReadSuper);
            }
            Callee::Import(i) => {
                return Err(Error::new(i.span, "dynamic import not supported", ""))
            }
            Callee::Expr(e) => {
                match e.as_ref() {
                    Expr::Array(_)
                    | Expr::Invalid(_)
                    | Expr::Lit(_)
                    | Expr::Object(_)
                    | Expr::TaggedTpl(_)
                    | Expr::Tpl(_)
                    | Expr::Unary(_)
                    | Expr::Update(_) => {
                        return Err(Error::new(
                            e.span(),
                            "call on non function values.",
                            "use a function instead.",
                        ))
                    }
                    Expr::Class(c) => {
                        return Err(Error::new(
                            c.span(),
                            format!(
                                "Class constructor {} cannot be invoked without 'new'",
                                c.ident.as_ref().map(|i| i.sym.as_ref()).unwrap_or("")
                            ),
                            "usea function instead.",
                        ))
                    }
                    Expr::Ident(i) => {
                        if let Some(v) = container.read_var(&i.sym) {
                            if v.is_class() {
                                return Err(Error::new(
                                    i.span,
                                    format!(
                                        "Class constructor {} cannot be invoked without 'new'",
                                        i.sym
                                    ),
                                    "use function instead.",
                                ));
                            }

                            if let Variable::Function(_id, funcid) = v {
                                if let Some(f) = self.functions.get(&funcid) {
                                    // async function and generater cannot be called statically
                                    if !f.is_async && !f.is_generator {
                                        for i in &f.captures {
                                            container._add_heap_variable(*i);
                                        }

                                        is_static_call = Some(funcid);
                                    }
                                } else {
                                    // the function is not yet defined
                                    maybe_static_call = Some(funcid);
                                }
                            };

                            // read variable if not static
                            if is_static_call.is_none() {
                                container.push(IR::ReadVar(v.id()));
                            }
                        } else {
                            return Err(Error::new(
                                i.span,
                                format!("Cannot find identifier '{}'.", i.sym),
                                "",
                            ));
                        }
                    }
                    Expr::Member(_) => {
                        is_member_call = true;
                        self.translate_expr(container, &e)?;
                    }
                    Expr::Arrow(a) => {
                        let funcid = self.translate_arrow(container, a)?;

                        // async and generator cannot be called statically
                        if !a.is_async && !a.is_generator {
                            is_static_call = Some(funcid);
                        }
                    }
                    Expr::Fn(f) => {
                        let funcid = FunctionId::new();
                        self.translate_function(container, &f.function, funcid, None)?;

                        // async and generator cannot be called statically
                        if !f.function.is_async && !f.function.is_generator {
                            is_static_call = Some(funcid);
                        }
                    }
                    _ => self.translate_expr(container, &e)?,
                };
            }
        };

        if let Some(funcid) = is_static_call {
            if is_dynamic_call {
                container.push(IR::CallStaticVarArgs {
                    func_id: funcid,
                    var_arg: var_arg_id,
                });
            } else {
                container.push(IR::CallStatic {
                    func_id: funcid,
                    arg_len,
                    args: args_list,
                });
            }
        } else if is_member_call {
            if is_dynamic_call {
                container.push(IR::ObjCallVarArg {
                    var_arg: var_arg_id,
                })
            } else {
                container.push(IR::ObjCall {
                    args: args_list,
                    arg_len,
                })
            }
        } else {
            if is_dynamic_call {
                container.push(IR::CallVarArgs {
                    var_arg: var_arg_id,
                });
            } else {
                container.push(IR::Call {
                    arg_len: arg_len,
                    args: args_list,
                    maybe_static: maybe_static_call,
                });
            }
        }

        return Ok(());
    }
}
