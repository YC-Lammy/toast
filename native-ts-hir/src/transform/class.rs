use native_js_common::error::Error;

use native_ts_parser::swc_core::common::{Span, Spanned};
use native_ts_parser::swc_core::ecma::ast as swc;

use super::Transformer;

use crate::ast::{
    ClassType, Expr, FuncType, FunctionParam, PropNameOrExpr, PropertyDesc, Stmt, Type,
};
use crate::common::{ClassId, FunctionId, VariableId};
use crate::transform::context::Binding;

type Result<T> = std::result::Result<T, Error<Span>>;

impl Transformer {
    /// this function only translates type definitions and does not translate any initialiser or methods
    pub fn translate_class_ty(&mut self, id: ClassId, class: &swc::Class) -> Result<ClassType> {
        let mut class_ty = ClassType::default();

        if let Some(_type_params) = &class.type_params {
            todo!("generic class")
        }

        // translate super class
        if let Some(super_expr) = &class.super_class {
            // translate super args
            let super_args = if let Some(super_type_args) = &class.super_type_params {
                self.translate_type_args(&super_type_args)?
            } else {
                Vec::new()
            };
            let super_ty = self.translate_expr_type(&super_expr, &super_args)?;

            match super_ty {
                Type::Object(class_id) => {
                    class_ty.extends = Some(class_id);
                }
                _ => {
                    return Err(Error::syntax_error(
                        super_expr.span(),
                        "class can only extend a class",
                    ))
                }
            }
        }

        for member in &class.body {
            match member {
                swc::ClassMember::AutoAccessor(a) => {
                    return Err(Error::syntax_error(a.span, "auto accessor not supported"))
                }
                swc::ClassMember::Empty(_) => {}
                // ignore private method as it is not exposed
                swc::ClassMember::PrivateMethod(_) => {}
                // ignore private prop as it is not exposed
                swc::ClassMember::PrivateProp(_) => {}
                // ignored as it is not evaluated
                swc::ClassMember::StaticBlock(_) => {}
                // index signature will not be supported
                swc::ClassMember::TsIndexSignature(i) => {
                    return Err(Error::syntax_error(
                        i.span,
                        "index signature not allowed, use a map instead",
                    ))
                }
                swc::ClassMember::Constructor(c) => {
                    // create function id
                    let dummy_id = FunctionId::new();
                    // create function type
                    let mut constructor_ty = FuncType {
                        this_ty: Type::Object(id),
                        params: Vec::new(),
                        var_arg: false,
                        return_ty: Type::Undefined,
                    };

                    // translate params
                    for p in &c.params {
                        match p {
                            swc::ParamOrTsParamProp::Param(p) => {
                                // identifier binding
                                if let Some(ident) = p.pat.as_ident() {
                                    // translate type annotation
                                    if let Some(ann) = &ident.type_ann {
                                        // translate type
                                        let mut ty = self.translate_type(&ann.type_ann)?;

                                        if ident.optional {
                                            ty = ty.union(Type::Any);
                                        }
                                        // push param to type
                                        constructor_ty.params.push(ty);
                                    } else {
                                        // missing type annotation
                                        return Err(Error::syntax_error(
                                            ident.span,
                                            "missing type annotation",
                                        ));
                                    }
                                } else if let Some(r) = p.pat.as_rest() {
                                    // variable argument
                                    return Err(Error::syntax_error(
                                        r.dot3_token,
                                        "variable arguments not supported",
                                    ));
                                } else {
                                    // destructive binding not supported
                                    return Err(Error::syntax_error(
                                        p.span,
                                        "destructive param not supported",
                                    ));
                                }
                            }
                            swc::ParamOrTsParamProp::TsParamProp(p) => {
                                match &p.param {
                                    swc::TsParamPropParam::Assign(a) => {
                                        // default argument not supported
                                        return Err(Error::syntax_error(
                                            a.span,
                                            "default param not supported",
                                        ));
                                    }
                                    swc::TsParamPropParam::Ident(ident) => {
                                        // translate type annotation
                                        if let Some(ann) = &ident.type_ann {
                                            // translate type
                                            let mut ty = self.translate_type(&ann.type_ann)?;

                                            if ident.optional {
                                                ty = ty.union(Type::Any);
                                            }
                                            // push param type
                                            constructor_ty.params.push(ty);
                                        } else {
                                            // missing type annotation
                                            return Err(Error::syntax_error(
                                                ident.span,
                                                "missing type annotation",
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // set constructor type
                    class_ty.constructor = Some((dummy_id, constructor_ty));
                }
                swc::ClassMember::ClassProp(prop) => {
                    let ty = if let Some(ann) = &prop.type_ann {
                        let ty = self.translate_type(&ann.type_ann)?;
                        // optional
                        if prop.is_optional {
                            ty.union(Type::Undefined)
                        } else {
                            ty
                        }
                    } else {
                        Type::Any
                    };

                    let name = self.translate_prop_name(&prop.key)?;
                    let name = match name {
                        PropNameOrExpr::Expr(_, _) => {
                            return Err(Error::syntax_error(
                                prop.key.span(),
                                "dynamic propname is not allowed",
                            ))
                        }
                        PropNameOrExpr::PropName(p) => p,
                    };

                    if prop.is_static {
                        let id = VariableId::new();
                        self.context.declare_global(id, ty.clone());
                        class_ty.static_properties.insert(name, (id, ty));
                    } else {
                        class_ty.properties.insert(
                            name,
                            PropertyDesc {
                                ty: ty,
                                readonly: prop.readonly,
                                initialiser: None,
                            },
                        );
                    }
                }
                swc::ClassMember::Method(m) => {
                    let mut func_ty = self.translate_function_ty(&m.function)?;
                    let prop = self.translate_prop_name(&m.key)?;
                    let prop = match prop {
                        PropNameOrExpr::Expr(..) => {
                            return Err(Error::syntax_error(
                                m.key.span(),
                                "dynamic prop name not allowed",
                            ))
                        }
                        PropNameOrExpr::PropName(p) => p,
                    };

                    if m.is_optional {
                        return Err(Error::syntax_error(
                            m.span,
                            "class method cannot be optional",
                        ));
                    }

                    if m.is_static {
                        class_ty
                            .static_methods
                            .insert(prop, (FunctionId::new(), func_ty));
                    } else {
                        func_ty.this_ty = Type::Object(id);
                        class_ty.methods.insert(prop, (FunctionId::new(), func_ty));
                    }
                }
            }
        }

        return Ok(class_ty);
    }

    pub fn translate_class(&mut self, id: ClassId, name: String, class: &swc::Class) -> Result<()> {
        let mut class_ty = self
            .context
            .classes
            .get(&id)
            .cloned()
            .unwrap_or(ClassType::default());

        class_ty.name = name;

        if let Some(_type_params) = &class.type_params {
            todo!("generic class")
        }

        // translate super class
        if let Some(super_expr) = &class.super_class {
            // translate super args
            let super_args = if let Some(super_type_args) = &class.super_type_params {
                self.translate_type_args(&super_type_args)?
            } else {
                Vec::new()
            };
            let super_ty = self.translate_expr_type(&super_expr, &super_args)?;

            match super_ty {
                Type::Object(class_id) => {
                    class_ty.extends = Some(class_id);
                }
                _ => {
                    return Err(Error::syntax_error(
                        super_expr.span(),
                        "class can only extend a class",
                    ))
                }
            }
        }

        for i in &class.implements {
            // translate super args
            let type_args = if let Some(type_args) = &i.type_args {
                self.translate_type_args(&type_args)?
            } else {
                Vec::new()
            };
            let impl_ty = self.translate_expr_type(&i.expr, &type_args)?;

            match impl_ty {
                Type::Interface(iface) => {
                    class_ty.implements.push(iface);
                    // pend a future type check
                    self.type_checks.push(super::TypeCheck {
                        span: i.span,
                        ty: Type::Object(id),
                        fulfills: Type::Interface(iface),
                    });
                }
                _ => {
                    return Err(Error::syntax_error(
                        i.span,
                        "class can only implement interfaces",
                    ))
                }
            }
        }

        for member in &class.body {
            match member {
                swc::ClassMember::AutoAccessor(a) => {
                    // auto accessor not supported
                    return Err(Error::syntax_error(a.span, "auto accessor not supported"));
                }
                swc::ClassMember::TsIndexSignature(i) => {
                    // index signature not supported
                    return Err(Error::syntax_error(
                        i.span,
                        "index signature not allowed, use a map instead",
                    ));
                }
                swc::ClassMember::Empty(_) => {}
                swc::ClassMember::StaticBlock(block) => {
                    // translate block directly
                    self.translate_block_stmt(&block.body, None)?;
                }
                swc::ClassMember::ClassProp(prop) => {
                    let ty = if let Some(ann) = &prop.type_ann {
                        let ty = self.translate_type(&ann.type_ann)?;
                        // optional
                        if prop.is_optional {
                            ty.union(Type::Undefined)
                        } else {
                            ty
                        }
                    } else {
                        Type::Any
                    };

                    let name = self.translate_prop_name(&prop.key)?;
                    let name = match name {
                        PropNameOrExpr::Expr(_, _) => {
                            return Err(Error::syntax_error(
                                prop.key.span(),
                                "dynamic propname is not allowed",
                            ))
                        }
                        PropNameOrExpr::PropName(p) => p,
                    };

                    let init = if let Some(e) = &prop.value {
                        let (e, _) = self.translate_expr(e, Some(&ty))?;
                        Some(e)
                    } else {
                        None
                    };

                    if prop.is_static {
                        let id = VariableId::new();
                        self.context.declare_global(id, ty.clone());
                        class_ty.static_properties.insert(name, (id, ty));

                        if let Some(init) = init {
                            self.context.func().stmts.push(Stmt::Expr(Expr::VarAssign {
                                op: crate::ast::AssignOp::Assign,
                                variable: id,
                                value: Box::new(init),
                            }));
                        }
                    } else {
                        class_ty.properties.insert(
                            name,
                            PropertyDesc {
                                ty: ty,
                                readonly: prop.readonly,
                                initialiser: init,
                            },
                        );
                    }
                }
                swc::ClassMember::PrivateProp(prop) => {
                    let ty = if let Some(ann) = &prop.type_ann {
                        let ty = self.translate_type(&ann.type_ann)?;
                        // optional
                        if prop.is_optional {
                            ty.union(Type::Undefined)
                        } else {
                            ty
                        }
                    } else {
                        Type::Any
                    };

                    let name = crate::PropName::Private(prop.key.id.sym.to_string());

                    let init = if let Some(e) = &prop.value {
                        let (e, _) = self.translate_expr(e, Some(&ty))?;
                        Some(e)
                    } else {
                        None
                    };

                    if prop.is_static {
                        let id = VariableId::new();
                        self.context.declare_global(id, ty.clone());
                        class_ty.static_properties.insert(name, (id, ty));

                        if let Some(init) = init {
                            self.context.func().stmts.push(Stmt::Expr(Expr::VarAssign {
                                op: crate::ast::AssignOp::Assign,
                                variable: id,
                                value: Box::new(init),
                            }));
                        }
                    } else {
                        class_ty.properties.insert(
                            name,
                            PropertyDesc {
                                ty: ty,
                                readonly: prop.readonly,
                                initialiser: init,
                            },
                        );
                    }
                }
                swc::ClassMember::Constructor(c) => {
                    // use declared id or else create a new id
                    let constructor_id = class_ty
                        .constructor
                        .as_ref()
                        .map(|(id, _)| *id)
                        .unwrap_or(FunctionId::new());
                    // create function type
                    let mut constructor_ty = FuncType {
                        this_ty: Type::Object(id),
                        params: Vec::new(),
                        var_arg: false,
                        return_ty: Type::Undefined,
                    };

                    let old_this = core::mem::replace(&mut self.this_ty, Type::Object(id));
                    let old_return = core::mem::replace(&mut self.return_ty, Type::Undefined);
                    let old_is_constructor = core::mem::replace(&mut self.is_in_constructor, true);

                    self.context.new_function(constructor_id);

                    // translate params
                    for p in &c.params {
                        match p {
                            swc::ParamOrTsParamProp::Param(p) => {
                                // identifier binding
                                if let Some(ident) = p.pat.as_ident() {
                                    // translate type annotation
                                    if let Some(ann) = &ident.type_ann {
                                        // translate type
                                        let mut ty = self.translate_type(&ann.type_ann)?;

                                        if ident.optional {
                                            ty = ty.union(Type::Any);
                                        }
                                        // push param to type
                                        constructor_ty.params.push(ty.clone());

                                        // create new id
                                        let param_id = VariableId::new();
                                        // push param
                                        self.context.func().params.push(FunctionParam {
                                            ty: ty.clone(),
                                            id: param_id,
                                        });
                                        self.context.declare(
                                            &ident.sym,
                                            Binding::Var {
                                                writable: true,
                                                redeclarable: true,
                                                id: param_id,
                                                ty: ty,
                                            },
                                        );
                                    } else {
                                        // missing type annotation
                                        return Err(Error::syntax_error(
                                            ident.span,
                                            "missing type annotation",
                                        ));
                                    }
                                } else if let Some(r) = p.pat.as_rest() {
                                    // variable argument
                                    return Err(Error::syntax_error(
                                        r.dot3_token,
                                        "variable arguments not supported",
                                    ));
                                } else {
                                    // destructive binding not supported
                                    return Err(Error::syntax_error(
                                        p.span,
                                        "destructive param not supported",
                                    ));
                                }
                            }
                            swc::ParamOrTsParamProp::TsParamProp(p) => {
                                match &p.param {
                                    swc::TsParamPropParam::Assign(a) => {
                                        // default argument not supported
                                        return Err(Error::syntax_error(
                                            a.span,
                                            "default param not supported",
                                        ));
                                    }
                                    swc::TsParamPropParam::Ident(ident) => {
                                        // translate type annotation
                                        if let Some(ann) = &ident.type_ann {
                                            // translate type
                                            let mut ty = self.translate_type(&ann.type_ann)?;

                                            if ident.optional {
                                                ty = ty.union(Type::Any);
                                            }
                                            // push param type
                                            constructor_ty.params.push(ty.clone());

                                            // create new id
                                            let param_id = VariableId::new();
                                            // push param
                                            self.context.func().params.push(FunctionParam {
                                                ty: ty.clone(),
                                                id: param_id,
                                            });
                                            self.context.declare(
                                                &ident.sym,
                                                Binding::Var {
                                                    writable: true,
                                                    redeclarable: true,
                                                    id: param_id,
                                                    ty: ty,
                                                },
                                            );
                                        } else {
                                            // missing type annotation
                                            return Err(Error::syntax_error(
                                                ident.span,
                                                "missing type annotation",
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if let Some(body) = &c.body {
                        self.translate_block_stmt(body, None)?;
                    } else {
                        return Err(Error::syntax_error(c.span, "missing constructor body"));
                    }

                    let func_id = self.context.end_function();
                    debug_assert!(func_id == constructor_id);

                    self.this_ty = old_this;
                    self.return_ty = old_return;
                    self.is_in_constructor = old_is_constructor;

                    // set constructor type
                    class_ty.constructor = Some((constructor_id, constructor_ty));
                }
                swc::ClassMember::Method(m) => {
                    let prop = self.translate_prop_name(&m.key)?;
                    let prop = match prop {
                        PropNameOrExpr::Expr(..) => {
                            return Err(Error::syntax_error(
                                m.key.span(),
                                "dynamic prop name not allowed",
                            ))
                        }
                        PropNameOrExpr::PropName(p) => p,
                    };

                    let method_id = if m.is_static {
                        class_ty
                            .static_methods
                            .get(&prop)
                            .map(|(i, _)| *i)
                            .unwrap_or(FunctionId::new())
                    } else {
                        class_ty
                            .methods
                            .get(&prop)
                            .map(|(i, _)| *i)
                            .unwrap_or(FunctionId::new())
                    };

                    if m.is_optional {
                        return Err(Error::syntax_error(
                            m.span,
                            "class method cannot be optional",
                        ));
                    }

                    self.translate_function(method_id, Some(Type::Object(id)), &m.function)?;
                    let func_ty = self
                        .context
                        .functions
                        .get(&method_id)
                        .expect("invalid function")
                        .ty();

                    if m.is_static {
                        class_ty.static_methods.insert(prop, (method_id, func_ty));
                    } else {
                        class_ty.methods.insert(prop, (method_id, func_ty));
                    }
                }
                swc::ClassMember::PrivateMethod(m) => {
                    let prop = crate::PropName::Private(m.key.id.sym.to_string());

                    let method_id = if m.is_static {
                        class_ty
                            .static_methods
                            .get(&prop)
                            .map(|(i, _)| *i)
                            .unwrap_or(FunctionId::new())
                    } else {
                        class_ty
                            .methods
                            .get(&prop)
                            .map(|(i, _)| *i)
                            .unwrap_or(FunctionId::new())
                    };

                    if m.is_optional {
                        return Err(Error::syntax_error(
                            m.span,
                            "class method cannot be optional",
                        ));
                    }

                    self.translate_function(method_id, Some(Type::Object(id)), &m.function)?;
                    let func_ty = self
                        .context
                        .functions
                        .get(&method_id)
                        .expect("invalid function")
                        .ty();

                    if m.is_static {
                        class_ty.static_methods.insert(prop, (method_id, func_ty));
                    } else {
                        class_ty.methods.insert(prop, (method_id, func_ty));
                    }
                }
            }
        }

        self.context.classes.insert(id, class_ty);
        return Ok(());
    }
}
