use super::*;

impl IRBuilder {
    pub fn assign_acc_to_variable(
        &mut self,
        container: &mut dyn IRContainer,
        name: &JsWord,
        kind: Option<VarDeclKind>,
        span: Span,
    ) -> Result<(), Error> {
        // read variable description with the provided name
        let v = container.read_var(name);

        if let Some(v) = v {
            // a class cannot be reassigned
            if v.is_class() {
                return Err(Error::new(
                    span,
                    format!("Cannot assign to '{}' because it is a class.", name),
                    "",
                ));
            }

            // a function cannot be reassigned
            if v.is_function() {
                return Err(Error::new(
                    span,
                    format!("Cannot assign to '{}' because it is a function.", name),
                    "",
                ));
            }

            // a const cannot be reassigned
            if v.is_const() && kind != Some(VarDeclKind::Const) {
                return Err(Error::new(
                    span,
                    format!("Cannot assign to '{}' because it is a const.", name),
                    "",
                ));
            }
            // write value to variable
            container.push(IR::WriteVar(v.id()));
        } else {
            return Err(Error::new(
                span,
                format!("Cannot find identifier '{}'.", name),
                "",
            ));
        };

        return Ok(());
    }

    pub fn translate_assignment(
        &mut self,
        container: &mut dyn IRContainer,
        pat: &Pat,
        kind: Option<VarDeclKind>,
    ) -> Result<(), Error> {
        match pat {
            Pat::Array(a) => {
                // store the object on temp
                let tempid = TempId::new();
                container.push(IR::StoreTemp(tempid));

                let mut i = 0;
                for elem in &a.elems {
                    // only perform assignment if a pattern is provided, otherwise skip index
                    if let Some(elem) = elem {
                        match elem {
                            // if the pattern is a rest pattern, we iterate the object
                            Pat::Rest(r) => {
                                // create an iterator of the target object
                                let iterid = IterId::new();
                                container.push(IR::LoadTemp(tempid));
                                container.push(IR::CreateForOfIter(iterid));

                                // create an empty array
                                let array_id = TempId::new();
                                container.push(IR::CreateArray { size: 0 });
                                container.push(IR::StoreTemp(array_id));

                                // loop until iterator is done
                                container.push(IR::Loop { label: None });
                                container.push(IR::BreakIfIterDone(iterid));

                                // read next item
                                container.push(IR::IterNext(iterid));
                                // push to array
                                container.push(IR::ArrayPush { array: array_id });

                                // end of loop
                                container.push(IR::EndLoop);
                                container.push(IR::DropIterator(iterid));

                                // release temp
                                container.push(IR::LoadTemp(array_id));
                                container.push(IR::DropTemp(array_id));

                                // assign array to pattern
                                self.translate_assignment(container, &r.arg, kind)?;
                            }
                            // otherwise we read the index from object
                            elem => {
                                container.push(IR::LoadTemp(tempid));
                                container.push(IR::ReadIndex { index: i });
                                self.translate_assignment(container, elem, kind)?;
                            }
                        };
                    };
                    // increment index
                    i += 1;
                }

                // release the temp
                container.push(IR::DropTemp(tempid));
            }
            Pat::Assign(a) => {
                let tempid = TempId::new();

                // push the value to stack
                container.push(IR::StoreTemp(tempid));

                // translate the default value
                self.translate_expr(container, &a.right)?;

                // check if the stack value is undefined and select
                container.push(IR::NonUndefine(tempid));

                // assign the selected value
                self.translate_assignment(container, &a.left, kind)?;
            }
            Pat::Ident(i) => {
                self.assign_acc_to_variable(container, &i.sym, kind, i.span)?;
            }

            Pat::Invalid(i) => return Err(Error::new(i.span, "invalid assignment.", "")),
            Pat::Object(o) => {
                // store the object to temp
                let tempid = TempId::new();
                container.push(IR::StoreTemp(tempid));

                for prop in &o.props {
                    match prop {
                        ObjectPatProp::KeyValue(k) => {
                            let propname = match &k.key {
                                PropName::Ident(i) => Some(i.sym.clone()),
                                PropName::Str(s) => Some(s.value.clone()),
                                PropName::BigInt(b) => {
                                    Some(JsWord::from(b.value.to_string() + "n"))
                                }
                                PropName::Num(n) => Some(JsWord::from(n.value.to_string())),
                                PropName::Computed(c) => {
                                    if let Some(lit) = c.expr.as_lit() {
                                        match lit {
                                            Lit::Null(_) => Some(JsWord::from("null")),
                                            Lit::Bool(b) => Some(JsWord::from(
                                                b.value.then_some("true").unwrap_or("false"),
                                            )),
                                            Lit::BigInt(b) => {
                                                Some(JsWord::from(b.value.to_string()))
                                            }

                                            Lit::Num(n) => Some(JsWord::from(n.value.to_string())),
                                            Lit::Str(s) => Some(s.value.clone()),
                                            Lit::Regex(r) => Some(JsWord::from(format!(
                                                "/{}/{}",
                                                r.exp, r.flags
                                            ))),
                                            Lit::JSXText(_) => unimplemented!(),
                                        }
                                    } else {
                                        self.translate_expr(container, &c.expr)?;
                                        None
                                    }
                                }
                            };

                            if let Some(propname) = propname {
                                // load the object
                                container.push(IR::LoadTemp(tempid));
                                // read field
                                container.push(IR::ReadField { key: propname });
                            } else {
                                container.push(IR::ReadComputed { obj: tempid })
                            };

                            self.translate_assignment(container, &k.value, kind)?;
                        }
                        ObjectPatProp::Assign(a) => {
                            // load the object
                            container.push(IR::LoadTemp(tempid));
                            // read the field
                            container.push(IR::ReadField {
                                key: a.key.sym.clone(),
                            });

                            if let Some(default_value) = &a.value {
                                let read_field = TempId::new();

                                // push the readed field value to stack
                                container.push(IR::StoreTemp(read_field));

                                // translate the default value to ACC
                                self.translate_expr(container, &default_value)?;

                                // select the non-undefined value
                                container.push(IR::NonUndefine(read_field));
                                container.push(IR::DropTemp(read_field));
                            }

                            // read variable description with the provided name
                            self.assign_acc_to_variable(container, &a.key.sym, kind, a.key.span)?;
                        }
                        ObjectPatProp::Rest(r) => {
                            // todo!()
                            container.push(IR::LoadTemp(tempid));
                            self.translate_assignment(container, &r.arg, kind)?;
                        }
                    }
                }

                container.push(IR::DropTemp(tempid));
            }
            Pat::Rest(_) => {
                unreachable!()
            }
            Pat::Expr(_) => {
                unreachable!()
            }
        };

        return Ok(());
    }

    pub fn translate_assign_expr(
        &mut self,
        container: &mut dyn IRContainer,
        assign_expr: &AssignExpr,
    ) -> Result<(), Error> {
        self.translate_expr(container, &assign_expr.right)?;

        // pure assignment
        if assign_expr.op == AssignOp::Assign {
            match &assign_expr.left {
                PatOrExpr::Expr(e) => {
                    // create a clousure to handle member assign
                    fn handle_member(
                        m: &MemberExpr,
                        container: &mut dyn IRContainer,
                        this: &mut IRBuilder,
                    ) -> Result<(), Error> {
                        let value_id = TempId::new();
                        container.push(IR::StoreTemp(value_id));

                        this.translate_expr(container, &m.obj)?;
                        let obj_id = TempId::new();
                        container.push(IR::StoreTemp(obj_id));

                        match &m.prop {
                            MemberProp::Ident(ident) => {
                                container.push(IR::LoadTemp(value_id));
                                container.push(IR::WriteField {
                                    object: obj_id,
                                    key: ident.sym.clone(),
                                });
                            }
                            MemberProp::PrivateName(p) => {
                                container.push(IR::LoadTemp(value_id));
                                container.push(IR::WriteField {
                                    object: obj_id,
                                    key: JsWord::from(format!("#{}", p.id.sym.clone())),
                                });
                            }
                            MemberProp::Computed(computed_propname) => {
                                if let Some(lit) = computed_propname.expr.as_lit() {
                                    match lit {
                                        Lit::BigInt(b) => container.push(IR::WriteField {
                                            object: obj_id,
                                            key: JsWord::from(b.value.to_string() + "n"),
                                        }),
                                        Lit::Bool(b) => container.push(IR::WriteField {
                                            object: obj_id,
                                            key: JsWord::from(
                                                b.value.then_some("true").unwrap_or("false"),
                                            ),
                                        }),
                                        Lit::JSXText(_) => unimplemented!(),
                                        Lit::Null(_n) => container.push(IR::WriteField {
                                            object: obj_id,
                                            key: JsWord::from("null"),
                                        }),
                                        Lit::Num(n) => {
                                            if n.value as usize as f64 == n.value && n.value >= 0.0
                                            {
                                                container.push(IR::WriteIndex {
                                                    object: obj_id,
                                                    index: n.value as usize,
                                                })
                                            } else {
                                                container.push(IR::WriteField {
                                                    object: obj_id,
                                                    key: JsWord::from(n.value.to_string()),
                                                })
                                            }
                                        }
                                        Lit::Regex(r) => container.push(IR::WriteField {
                                            object: obj_id,
                                            key: JsWord::from(format!("/{}/{}", r.exp, r.flags)),
                                        }),
                                        Lit::Str(s) => container.push(IR::WriteField {
                                            object: obj_id,
                                            key: s.value.clone(),
                                        }),
                                    }
                                } else {
                                    // translate the propname
                                    this.translate_expr(container, &computed_propname.expr)?;
                                    let propname_id = TempId::new();
                                    container.push(IR::StoreTemp(propname_id));

                                    // load value to ACC
                                    container.push(IR::LoadTemp(value_id));
                                    container.push(IR::WriteComputed {
                                        obj: obj_id,
                                        propname: propname_id,
                                    });
                                    container.push(IR::DropTemp(propname_id));
                                }
                            }
                        };

                        container.push(IR::DropTemp(obj_id));
                        return Ok(());
                    }

                    // handle different member expressions
                    match e.as_ref() {
                        Expr::Member(m) => {
                            // assign the value
                            handle_member(m, container, self)?;
                        }
                        Expr::OptChain(o) => {
                            match o.base.as_ref() {
                                OptChainBase::Member(m) => {
                                    let value_id = TempId::new();
                                    // store the value
                                    container.push(IR::StoreTemp(value_id));
                                    // translate the object
                                    self.translate_expr(container, &m.obj)?;

                                    // if the object is not undefined
                                    container.push(IR::If);
                                    // read the value
                                    container.push(IR::LoadTemp(value_id));
                                    // assign the value
                                    handle_member(m, container, self)?;

                                    container.push(IR::EndIf);

                                    container.push(IR::DropTemp(value_id));
                                }
                                OptChainBase::Call(_) => unreachable!(),
                            }
                        }
                        _ => unimplemented!(),
                    }
                }
                PatOrExpr::Pat(p) => {
                    self.translate_assignment(container, &p, None)?;
                }
            };
        } else {
            let value_id = TempId::new();
            // store the value
            container.push(IR::StoreTemp(value_id));

            // get the original value
            match &assign_expr.left {
                PatOrExpr::Expr(e) => self.translate_expr(container, &e)?,
                _ => unimplemented!(),
            };

            // get the op code
            let ir = match assign_expr.op {
                AssignOp::AddAssign => IR::Add,
                AssignOp::AndAssign => IR::And,
                AssignOp::BitAndAssign => IR::BitAnd,
                AssignOp::BitOrAssign => IR::BitOr,
                AssignOp::BitXorAssign => IR::BitXor,
                AssignOp::DivAssign => IR::Div,
                AssignOp::ExpAssign => IR::Exp,
                AssignOp::LShiftAssign => IR::LShift,
                AssignOp::ModAssign => IR::Mod,
                AssignOp::MulAssign => IR::Mul,
                AssignOp::NullishAssign => IR::Nullish,
                AssignOp::RShiftAssign => IR::RShift,
                AssignOp::SubAssign => IR::Sub,
                AssignOp::ZeroFillRShiftAssign => IR::ZeroFillRShift,
                AssignOp::OrAssign => IR::Or,
                AssignOp::Assign => unreachable!(),
            };

            // perform the operation
            container.push(ir(value_id));

            // assign the oped value to target
            match &assign_expr.left {
                PatOrExpr::Expr(e) => {
                    match e.as_ref() {
                        Expr::Member(_m) => {
                            // assign to the previous field directly
                            container.push(IR::ObjAssign);
                        }
                        Expr::OptChain(_c) => {
                            container.push(IR::ObjAssign);
                        }
                        Expr::Ident(id) => {
                            self.assign_acc_to_variable(container, &id.sym, None, id.span)?;
                        }
                        _ => unimplemented!(),
                    }
                }
                PatOrExpr::Pat(p) => {
                    self.translate_assignment(container, &p, None)?;
                }
            };

            container.push(IR::DropTemp(value_id));
        }

        return Ok(());
    }
}
