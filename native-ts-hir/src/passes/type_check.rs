use std::collections::HashMap;

use native_js_common::{error::Error, rc::Rc};
use swc_common::Span;

use crate::{
    passes::generic_function_resolver::GenericFunctionResolver,
    untyped_hir::{
        visit::{BreakOrContinue, Visitor},
        AssignOp, BinOp, Callee, ClassMethod, Expr, FunctionType, MemberOrVar, Type,
    },
    PropName, VarId,
};

pub struct TypeChecker {
    variables: HashMap<VarId, Type>,
    super_type: Option<Type>,
    this_type: Type,
    return_type: Type,
    acc_types: Vec<Type>,
}

impl TypeChecker {
    fn trace_member(
        &self,
        span: Span,
        obj: &mut Type,
        prop: &PropName,
        type_args: &[Type],
    ) -> Result<Type, Error<Span>> {
        match obj {
            Type::Alias { .. } => panic!("unresolved alias"),
            Type::Generic(_) => panic!("unresolved generic"),
            Type::Unknown { span, .. } => {
                return Err(Error::syntax_error(
                    *span,
                    "cannot infer type, type annotation required",
                ))
            }
            Type::Any | Type::Undefined | Type::Null => {
                return Err(Error::syntax_error(
                    span,
                    format!("type '{}' has no property '{}'", obj, prop),
                ))
            }
            Type::Class {
                span: _,
                type_args: class_ty_args,
                class,
            } => {
                // gnerics should be resolved
                debug_assert!(class_ty_args.is_none());

                // find attribute
                if let Some(attr) = class.attributes.iter().find(|a| a.name.eq(prop)) {
                    // attributes should not have type arguments
                    if !type_args.is_empty() {
                        return Err(Error::syntax_error(
                            span,
                            format!("property '{}' expected 0 type arguments", prop),
                        ));
                    }

                    return Ok(attr.ty.clone());
                }

                // create a new propname for method
                let propname = if !type_args.is_empty() {
                    let mut id = prop.to_string();
                    id.push_str("<");

                    for ty in type_args.iter() {
                        id.push_str(&ty.to_string())
                    }

                    id.push_str(">");

                    PropName::Ident(id)
                } else {
                    prop.clone()
                };

                // find method
                if let Some(m) = class.methods.iter_mut().find(|m| m.name.eq(&propname)) {
                    if !m.function.ty.generics.is_empty() {
                        return Err(Error::syntax_error(span, "missing type arguments"));
                    }

                    return Ok(Type::Function {
                        type_args: None,
                        func: m.function.ty.clone(),
                    });
                }

                // may have to resolve the function
                if !type_args.is_empty() {
                    // find the non typed function
                    if let Some(m) = class.methods.iter_mut().find(|m| m.name.eq(prop)) {
                        if type_args.len() > m.function.ty.generics.len() {
                            return Err(Error::syntax_error(
                                span,
                                format!(
                                    "property '{}' expected {} type arguments",
                                    propname,
                                    m.function.ty.generics.len()
                                ),
                            ));
                        }

                        // resolve the generic function
                        let mut resolver = GenericFunctionResolver::default();

                        // resolve the function with arguments
                        if let Some(func) =
                            resolver.resolve_function_with_args(span, &mut m.function, type_args)?
                        {
                            // clone the function type
                            let ty = func.ty.clone();

                            // push the method to class
                            class.methods.push(ClassMethod {
                                name: propname,
                                function: func,
                            });

                            // return the new function type
                            return Ok(Type::Function {
                                type_args: None,
                                func: ty,
                            });
                        }

                        // the function does not have to be replaced
                        return Ok(Type::Function {
                            type_args: None,
                            func: m.function.ty.clone(),
                        });
                    }
                }

                // try to find it from super class
                if let Some(e) = &mut class.extends {
                    if let Ok(ty) = self.trace_member(span, e, prop, type_args) {
                        return Ok(ty);
                    }
                }

                return Err(Error::syntax_error(
                    span,
                    format!("class '{}' has no property '{}'", class.name, propname),
                ));
            }
            Type::Interface {
                span: _,
                type_args: iface_ty_args,
                interface,
            } => {
                debug_assert!(iface_ty_args.is_none());

                if !type_args.is_empty() {
                    return Err(Error::syntax_error(
                        span,
                        format!("property '{}' expected 0 type arguments", prop),
                    ));
                }

                debug_assert!(interface.generics.is_empty());

                if let Some(prop) = interface.props.iter().find(|p| p.name.eq(prop)) {
                    if prop.optinal {
                        return Ok(prop.ty.clone().union(Type::Undefined));
                    }
                    return Ok(prop.ty.clone());
                }

                for e in interface.extends.iter_mut() {
                    if let Ok(ty) = self.trace_member(span, e, prop, &[]) {
                        return Ok(ty);
                    }
                }
            }
            Type::Enum(_e) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }

                return Err(Error::syntax_error(span, "Enum has no properties"));
            }
            Type::Union(u) => {}
            Type::Array(a) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }

                match prop {
                    PropName::Int(i) => {
                        // return the element type
                        if *i >= 0 {
                            return Ok(a.as_ref().clone());
                        }
                    }
                    PropName::Ident(id) => match id.as_str() {
                        _ => todo!(),
                    },
                    _ => {}
                }
            }
            Type::Iterator(i) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Promise(p) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Regex => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::BigInt => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Int | Type::Number => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }

                match prop {
                    PropName::Ident(id) => match id.as_str() {
                        "toExponential" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::String,
                                }),
                            })
                        }
                        "toFixed" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::String,
                                }),
                            })
                        }
                        "toLocalString" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::String,
                                }),
                            })
                        }
                        "toPrecision" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::Number,
                                }),
                            })
                        }
                        "toString" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::String,
                                }),
                            })
                        }
                        "valueOf" => {
                            return Ok(Type::Function {
                                type_args: None,
                                func: Rc::new(FunctionType {
                                    visit_fingerprint: 0,
                                    is_definite: true,
                                    this_ty: Type::Number,
                                    generics: Vec::new(),
                                    params: Vec::new(),
                                    return_ty: Type::Number,
                                }),
                            })
                        }
                        _ => {}
                    },
                    _ => {}
                };
            }

            Type::String => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Symbol => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Bool => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Function {
                type_args: func_ty_args,
                func,
            } => {
                debug_assert!(func_ty_args.is_none());

                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::Map(t) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(span, "expected 0 type arguments"));
                }
            }
            Type::This => {
                let mut ty = self.this_type.clone();
                return self.trace_member(span, &mut ty, prop, type_args);
            }
            Type::Super => {
                if let Some(super_ty) = &self.super_type {
                    let ref mut super_ty = super_ty.clone();
                    return self.trace_member(span, super_ty, prop, type_args);
                } else {
                    return Err(Error::syntax_error(
                        span,
                        "'super' can only be referenced in class methods",
                    ));
                }
            }
            Type::Return => {
                let ref mut rty = self.return_type.clone();
                return self.trace_member(span, rty, prop, type_args);
            }
        };

        return Err(Error::syntax_error(
            span,
            format!("type '{}' has no property '{}'", obj, prop),
        ));
    }

    fn trace_call(
        &mut self,
        span: Span,
        callee_ty: &Type,
        args: &mut [Expr],
        is_optchain: bool,
        expected: Option<&Type>,
    ) -> Result<Type, Error<Span>> {
        let func_ty = match &callee_ty {
            Type::Function { type_args, func } => {
                debug_assert!(type_args.is_none());

                func.clone()
            }
            Type::Undefined | Type::Null if is_optchain => {
                // return directly as it will not call
                return Ok(callee_ty.clone());
            }
            Type::Union(u) if is_optchain => {
                let mut func_ty = None;

                for t in u {
                    match t {
                        Type::Undefined => {}
                        Type::Null => {}
                        Type::Function { type_args, func } => {
                            debug_assert!(type_args.is_none());

                            func_ty = Some(func.clone());
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                span,
                                format!(
                                    "callee have incompatable type, expected function found '{}'",
                                    callee_ty
                                ),
                            ))
                        }
                    }
                }

                if func_ty.is_none() {
                    // always nullish
                    return Ok(callee_ty.clone());
                }

                func_ty.unwrap()
            }
            _ => {
                return Err(Error::syntax_error(
                    span,
                    format!(
                        "callee have incompatable type, expected function found '{}'",
                        callee_ty
                    ),
                ))
            }
        };

        // check this type
        self.fulfills(&func_ty.this_ty, &self.this_type)?;

        // check param length
        if args.len() != func_ty.params.len() {
            return Err(Error::syntax_error(
                span,
                format!(
                    "function '{}' expected {} arguments, {} were given",
                    Type::Function {
                        type_args: None,
                        func: func_ty.clone()
                    },
                    func_ty.params.len(),
                    args.len()
                ),
            ));
        }
        // check arguments
        for (i, arg) in args.iter_mut().enumerate() {
            let _t = self.trace_type(arg, func_ty.params.get(i))?;
        }
        // check return type
        if let Some(expected) = expected {
            self.fulfills(expected, &func_ty.return_ty)?;
        }

        if is_optchain {
            return Ok(func_ty.return_ty.clone().union(Type::Undefined));
        }

        return Ok(func_ty.return_ty.clone());
    }

    fn fulfills(&self, expected: &Type, ty: &Type) -> Result<(), Error<Span>> {
        // return directly
        if expected == ty {
            return Ok(());
        }
        todo!()
    }

    pub fn trace_type(
        &mut self,
        expr: &mut Expr,
        expected: Option<&Type>,
    ) -> Result<Type, Error<Span>> {
        match expr {
            Expr::Array { span, values } => {
                let mut tys = Vec::new();
                // loop through elements
                for e in values.iter_mut() {
                    // trace element type
                    let t = self.trace_type(e, None)?;

                    // avoid duplicated types
                    if !tys.contains(&t) {
                        tys.push(t);
                    }
                }

                match expected {
                    Some(Type::Array(expected_elem)) => {
                        if tys.is_empty() {
                            return Ok(Type::Array(expected_elem.clone()));
                        }
                        let elem_ty = if tys.len() == 1 {
                            tys[0].clone()
                        } else {
                            Type::Union(tys)
                        };

                        // check if elem type fulfills
                        self.fulfills(&expected_elem, &elem_ty)?;

                        return Ok(Type::Array(Box::new(elem_ty)));
                    }
                    None => {
                        if tys.is_empty() {
                            return Err(Error::syntax_error(
                                *span,
                                "cannot infer element type for array, type annotation required",
                            ));
                        };

                        let elem_ty = if tys.len() == 1 {
                            tys[0].clone()
                        } else {
                            Type::Union(tys)
                        };

                        return Ok(Type::Array(Box::new(elem_ty)));
                    }
                    Some(expected) => {
                        if tys.is_empty() {
                            return Err(Error::syntax_error(
                                *span,
                                "cannot infer element type for array, type annotation required",
                            ));
                        };

                        let elem_ty = if tys.len() == 1 {
                            tys[0].clone()
                        } else {
                            Type::Union(tys)
                        };

                        let arr_ty = Type::Array(Box::new(elem_ty));

                        self.fulfills(&expected, &arr_ty)?;

                        return Ok(arr_ty);
                    }
                }
            }
            Expr::Assign {
                span,
                assign_op,
                target:
                    MemberOrVar::Var {
                        span: _,
                        name,
                        id,
                        ty,
                    },
                value,
            } => {
                if !self.variables.contains_key(&id) {
                    return Err(Error::syntax_error(
                        *span,
                        format!("variable '{}' use before declare", name),
                    ));
                };

                self.trace_type(value, Some(ty))?;

                if let Some(expected) = expected {
                    self.fulfills(expected, ty)?;
                }

                return Ok(ty.clone());
            }
            Expr::Assign {
                span,
                assign_op,
                target:
                    MemberOrVar::Member {
                        span: meber_span,
                        obj,
                        prop,
                    },
                value,
            } => {
                let value_ty = self.trace_type(value, None)?;

                let mut obj = self.trace_type(obj, None)?;

                let member_ty = self.trace_member(*meber_span, &mut obj, prop, &[])?;

                self.fulfills(&member_ty, &value_ty)?;

                if let Some(expected) = expected {
                    self.fulfills(expected, &member_ty)?;
                }

                return Ok(member_ty);
            }
            Expr::Assign {
                span,
                assign_op,
                target:
                    MemberOrVar::ClassMember {
                        span: class_span,
                        class,
                        prop,
                    },
                value,
            } => {
                if let Type::Class {
                    span: _,
                    type_args,
                    class,
                } = class
                {
                    debug_assert!(type_args.is_none());

                    if let Some(prop) = class.static_props.iter().find(|p| p.name.eq(prop)) {
                        let _value_ty = self.trace_type(value, Some(&prop.ty))?;

                        if let Some(expected) = expected {
                            self.fulfills(expected, &prop.ty)?;
                        }

                        return Ok(prop.ty.clone());
                    } else {
                        return Err(Error::syntax_error(
                            *class_span,
                            format!("class '{}' has no static property '{}'", class.name, prop),
                        ));
                    }
                } else {
                    return Err(Error::syntax_error(
                        *class_span,
                        "invalid left hand side assignment",
                    ));
                };
            }
            Expr::Await { span, value } => {
                let ty = self.trace_type(value, None)?;

                if let Type::Promise(p) = ty {
                    if let Some(expected) = expected {
                        self.fulfills(expected, &p)?;
                    }
                    return Ok(*p);
                } else {
                    if let Some(expected) = expected {
                        self.fulfills(expected, &ty)?;
                    }
                    return Ok(ty);
                }
            }
            Expr::BigInt(_) => {
                if let Some(expected) = &expected {
                    self.fulfills(expected, &Type::BigInt)?;
                }

                return Ok(Type::BigInt);
            }
            Expr::Bin {
                span,
                op,
                left,
                right,
            } => {
                let left_ty = self.trace_type(left, None)?;
                let right_ty = self.trace_type(right, None)?;

                let re_ty = match op {
                    BinOp::Add | BinOp::Div | BinOp::Exp | BinOp::Mod | BinOp::Mul | BinOp::Sub => {
                        if left_ty == Type::Int && right_ty == Type::Int {
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number {
                            Type::Number
                        } else if left_ty == Type::Number && right_ty == Type::Int {
                            Type::Number
                        } else if left_ty == Type::Int && right_ty == Type::Number {
                            Type::Number
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt {
                            Type::BigInt
                        } else if *op == BinOp::Add
                            && left_ty == Type::String
                            && right_ty == Type::String
                        {
                            Type::String
                        } else {
                            return Err(Error::syntax_error(
                                *span,
                                format!(
                                    "operation '{}' have incompatible types: {} and {}",
                                    op, left_ty, right_ty
                                ),
                            ));
                        }
                    }
                    BinOp::EqEq | BinOp::EqEqEq | BinOp::NotEq | BinOp::NotEqEq => {
                        if left_ty != right_ty {
                            return Err(Error::syntax_error(
                                *span,
                                format!(
                                    "operands of '{}' must be the same type, found {} and {}",
                                    op, left_ty, right_ty
                                ),
                            ));
                        }

                        Type::Bool
                    }
                    BinOp::And => Type::Bool,
                    BinOp::Nullish | BinOp::Or => Type::union(left_ty, right_ty),
                    BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::RShift
                    | BinOp::LShift
                    | BinOp::URShift => {
                        if left_ty == Type::Int && right_ty == Type::Int {
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number {
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Int {
                            Type::Int
                        } else if left_ty == Type::Int && right_ty == Type::Number {
                            Type::Int
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt {
                            Type::BigInt
                        } else {
                            return Err(Error::syntax_error(
                                *span,
                                format!(
                                    "operation '{}' have incompatible types: {} and {}",
                                    op, left_ty, right_ty
                                ),
                            ));
                        }
                    }
                    BinOp::Gt | BinOp::Gteq | BinOp::Lt | BinOp::Lteq => {
                        if left_ty == Type::Int && right_ty == Type::Int {
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number {
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Int {
                            Type::Int
                        } else if left_ty == Type::Int && right_ty == Type::Number {
                            Type::Int
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt {
                            Type::BigInt
                        } else {
                            return Err(Error::syntax_error(
                                *span,
                                format!(
                                    "operation '{}' have incompatible types: {} and {}",
                                    op, left_ty, right_ty
                                ),
                            ));
                        }
                    }
                    BinOp::In => {
                        if left_ty != Type::String {
                            return Err(Error::syntax_error(
                                *span,
                                "left-hand side of 'in' operator must have string type",
                            ));
                        }
                        Type::Bool
                    }
                };

                if let Some(expected) = &expected {
                    self.fulfills(expected, &re_ty)?;
                }

                return Ok(re_ty);
            }
            Expr::Bool(_) => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Bool)?;
                }
                Ok(Type::Bool)
            }
            Expr::Call {
                span,
                callee: Callee::Expr(calee_expr),
                is_optchain,
                type_args,
                args,
            } => {
                debug_assert!(type_args.is_empty());

                let callee_ty = self.trace_type(calee_expr, None)?;

                return self.trace_call(*span, &callee_ty, args, *is_optchain, expected);
            }
            Expr::Call {
                span,
                callee: Callee::Function(func),
                is_optchain: _,
                type_args,
                args,
            } => {
                debug_assert!(type_args.is_empty());

                // check param length
                if args.len() != func.ty.params.len() {
                    return Err(Error::syntax_error(
                        *span,
                        format!(
                            "function '{}' expected {} arguments, {} were given",
                            Type::Function {
                                type_args: None,
                                func: func.ty.clone()
                            },
                            func.ty.params.len(),
                            args.len()
                        ),
                    ));
                }

                // check this type
                self.fulfills(&func.ty.this_ty, &self.this_type)?;

                // check arguments
                for (i, arg) in args.iter_mut().enumerate() {
                    let t = self.trace_type(arg, func.ty.params.get(i))?;
                }

                // check return type
                if let Some(expected) = expected {
                    self.fulfills(expected, &func.ty.return_ty)?;
                }

                // return result type
                return Ok(func.ty.return_ty.clone());
            }
            Expr::Call {
                span,
                callee:
                    Callee::Member {
                        span: member_span,
                        obj,
                        prop,
                        is_optchain: _,
                    },
                is_optchain,
                type_args,
                args,
            } => {
                let mut obj_ty = self.trace_type(obj, None)?;

                let callee_ty = self.trace_member(*span, &mut obj_ty, prop, &type_args)?;

                let old_this = core::mem::replace(&mut self.this_type, obj_ty);

                let return_ty = self.trace_call(*span, &callee_ty, args, *is_optchain, expected)?;

                self.this_type = old_this;

                return Ok(return_ty);
            }
            Expr::Call {
                span,
                callee:
                    Callee::ClassMember {
                        span: class_span,
                        class,
                        prop,
                        is_optchain: _,
                    },
                is_optchain: _,
                type_args,
                args,
            } => {
                debug_assert!(type_args.is_empty());

                if let Type::Class {
                    span: _,
                    type_args,
                    class,
                } = class
                {
                    debug_assert!(type_args.is_none());
                } else {
                    return Err(Error::syntax_error(
                        *class_span,
                        "expected expression, found type",
                    ));
                }

                todo!()
            }
            Expr::Call {
                span,
                callee: Callee::Super,
                is_optchain: _,
                type_args,
                args,
            } => {
                debug_assert!(type_args.is_empty());

                if self.super_type.is_none() {
                    return Err(Error::syntax_error(
                        *span,
                        "super can only be called in constructor",
                    ));
                }

                todo!()
            }
            Expr::Cast { span, value, to_ty } => {
                let ty = self.trace_type(value, None)?;

                self.fulfills(&to_ty, &ty)?;

                return Ok(ty);
            }
            Expr::ClassMember {
                span,
                class,
                prop,
                type_args,
                is_optchain,
            } => {
                let ty = match class {
                    Type::Array(_) => {}
                    Type::Class {
                        span,
                        type_args,
                        class,
                    } => {}
                    Type::Enum(e) => {}
                    Type::Function { type_args, func } => {}
                    Type::Interface {
                        span,
                        type_args,
                        interface,
                    } => {}
                    Type::Iterator(i) => {}
                    _ => todo!(),
                };

                return Ok(Type::Undefined);
            }
            Expr::SuperMember { span, prop } => {
                if self.super_type.is_none() {
                    return Err(Error::syntax_error(
                        *span,
                        "'super' keyword can only be accessed inside class method.",
                    ));
                };

                let mut ty = self.super_type.as_ref().unwrap().clone();

                let member_ty = self.trace_member(*span, &mut ty, prop, &[])?;

                if let Some(expected) = expected {
                    self.fulfills(expected, &member_ty)?;
                }
                return Ok(member_ty);
            }
            Expr::Function {
                span,
                type_args,
                func,
            } => {
                debug_assert!(type_args.is_empty());

                let ty = Type::Function {
                    type_args: None,
                    func: func.ty.clone(),
                };
                if let Some(expected) = expected {
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty);
            }
            Expr::ImportMeta => todo!(),
            Expr::NewTarget => todo!(),
            Expr::InstanceOf { span, value, ty } => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Bool)?;
                }
                return Ok(Type::Bool);
            }
            Expr::Integer(_) => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Int)?;
                }
                Ok(Type::Int)
            }
            Expr::Member {
                span,
                obj,
                prop,
                type_args,
                is_optchain,
            } => {
                debug_assert!(type_args.is_empty());

                let mut obj_ty = self.trace_type(obj, None)?;

                let ty = self.trace_member(*span, &mut obj_ty, prop, &type_args)?;

                if let Some(expected) = expected {
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty);
            }
            Expr::New { span, callee, args } => {
                match callee {
                    Type::Class {
                        span,
                        type_args,
                        class,
                    } => {
                        debug_assert!(type_args.is_none());
                    }
                    _ => {
                        return Err(Error::syntax_error(
                            *span,
                            "new operator can only be called on a class",
                        ))
                    }
                };

                if let Some(expected) = expected {
                    self.fulfills(expected, callee)?;
                }
                return Ok(callee.clone());
            }
            Expr::Null => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Null)?;
                }
                Ok(Type::Null)
            }
            Expr::Number(_) => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Number)?;
                }
                Ok(Type::Number)
            }
            Expr::PrivateNameIn { span, name, value } => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Bool)?;
                }
                return Ok(Type::Bool);
            }
            Expr::ReadVar { span, name, id, ty } => {
                if !self.variables.contains_key(&id) {
                    return Err(Error::syntax_error(
                        *span,
                        format!("variable '{}' use before declare", name),
                    ));
                };

                if let Some(expected) = expected {
                    self.fulfills(expected, ty)?;
                }
                return Ok(ty.clone());
            }
            Expr::Regex { .. } => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Regex)?;
                }
                Ok(Type::Regex)
            }
            Expr::Seq { span, exprs } => {
                let mut ty = None;

                let len = exprs.len();

                for (i, e) in exprs.iter_mut().enumerate() {
                    if i == len - 1 {
                        ty = Some(self.trace_type(e, expected)?);
                    } else {
                        self.trace_type(e, None)?;
                    }
                }

                if let Some(ty) = ty {
                    if let Some(expected) = expected {
                        self.fulfills(expected, &ty)?;
                    }

                    return Ok(ty);
                } else {
                    return Ok(Type::Undefined);
                }
            }
            Expr::String(_) => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::String)?;
                }

                Ok(Type::String)
            }
            Expr::Ternary {
                span,
                test,
                left,
                right,
            } => {
                let test_ty = self.trace_type(test, None)?;
                let left_ty = self.trace_type(left, None)?;
                let right_ty = self.trace_type(right, None)?;

                let ty = left_ty.union(right_ty);

                if let Some(expected) = expected {
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty);
            }
            Expr::This(_) => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &self.this_type)?;
                }

                return Ok(self.this_type.clone());
            }
            Expr::Undefined => {
                if let Some(expected) = expected {
                    self.fulfills(expected, &Type::Undefined)?;
                }
                return Ok(Type::Undefined);
            }
            Expr::UnaryOp { span, op, value } => {
                todo!()
            }
            Expr::Update { span, target, op } => {
                todo!()
            }
            Expr::Yield {
                span,
                delegate,
                value,
            } => {
                todo!()
            }
        }
    }
}
