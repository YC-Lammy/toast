use std::collections::HashMap;

use native_js_common::error::Error;
use swc_common::Span;

use crate::{VarId, untyped_hir::{Type, visit::{Visitor, BreakOrContinue}, Expr, AssignOp, MemberOrVar, BinOp, Callee}, PropName};



pub struct TypeChecker{
    variables: HashMap<VarId, Type>,
    super_type: Option<Type>,
    this_type: Type,
    return_type: Type,
    acc_types: Vec<Type>,
}

impl TypeChecker{
    fn trace_member(&self, obj: &Type, prop: &PropName) -> Result<Type, Error<Span>>{
        match obj{
            Type::Alias { .. } => unreachable!(),
            Type::Unknown { .. } => todo!(),
            
        }
    }

    fn trace_call(&self, span:Span, callee_ty:&Type, args: &[Expr], is_optchain: bool, expected: Option<&Type>) -> Result<Type, Error<Span>>{
        let func_ty =
        match &callee_ty{
            Type::Function { type_args, func } => {
                debug_assert!(type_args.is_empty());

                func.clone()
            }
            Type::Undefined
            | Type::Null  if is_optchain => {
                // return directly as it will not call
                return Ok(callee_ty.clone())
            }
            Type::Union(u) if is_optchain => {
                
                let mut func_ty= None;

                for t in u{
                    match t{
                        Type::Undefined => {},
                        Type::Null => {},
                        Type::Function { type_args, func } => {
                            debug_assert!(type_args.is_empty());

                            func_ty = Some(func.clone());
                        }
                        _ => {
                            return Err(Error::syntax_error(span, format!("callee have incompatable type, expected function found '{}'", callee_ty)))
                        }
                    }
                };

                if func_ty.is_none(){
                    // always nullish
                    return Ok(callee_ty.clone())
                }

                func_ty.unwrap()
            },
            _ => {
                return Err(Error::syntax_error(span, format!("callee have incompatable type, expected function found '{}'", callee_ty)))
            }
        };

        // check this type
        self.fulfills(&func_ty.this_ty, &self.this_type)?;

        // check param length
        if args.len() != func_ty.params.len(){
            return Err(Error::syntax_error(span, format!("function '{}' expected {} arguments, {} were given", Type::Function{ type_args: Box::new([]), func: func_ty}, func_ty.params.len(), args.len())))
        }
        // check arguments
        for (i, arg) in args.iter().enumerate(){
            let t = self.trace_type(arg, func_ty.params.get(i))?;
        };
        // check return type
        if let Some(expected) = expected{
            self.fulfills(expected, &func_ty.return_ty)?;
        }

        if is_optchain{
            return Ok(func_ty.return_ty.clone().union(Type::Undefined))
        }

        return Ok(func_ty.return_ty.clone())
    }

    fn fulfills(&self, expected: &Type, ty: &Type) -> Result<(), Error<Span>>{
        // return directly
        if expected == ty{
            return Ok(())
        }
        todo!()
    }

    fn trace_type(&self, expr:&Expr, expected: Option<&Type>) -> Result<Type, Error<Span>>{
        match expr{
            Expr::Array { span, values } => {

                let mut tys = Vec::new();
                // loop through elements
                for e in values.iter(){
                    // trace element type
                    let t = self.trace_type(e, None)?;

                    // avoid duplicated types
                    if !tys.contains(&t){
                        tys.push(t);
                    }
                    
                }

                match expected{
                    Some(Type::Array(expected_elem)) => {
                        if tys.is_empty(){
                            return Ok(Type::Array(expected_elem.clone()))
                        }
                        let elem_ty = if tys.len() == 1{
                            tys[0].clone()
                        } else{
                            Type::Union(tys)
                        };

                        // check if elem type fulfills
                        self.fulfills(&expected_elem, &elem_ty)?;

                        return Ok(Type::Array(Box::new(elem_ty)))
                    }
                    None => {
                        if tys.is_empty(){
                            return Err(Error::syntax_error(*span, "cannot infer element type for array, type annotation required"))
                        };

                        let elem_ty = if tys.len() == 1{
                            tys[0].clone()
                        } else{
                            Type::Union(tys)
                        };

                        return Ok(Type::Array(Box::new(elem_ty)))
                    }
                    Some(expected) => {
                        if tys.is_empty(){
                            return Err(Error::syntax_error(*span, "cannot infer element type for array, type annotation required"))
                        };

                        let elem_ty = if tys.len() == 1{
                            tys[0].clone()
                        } else{
                            Type::Union(tys)
                        };

                        let arr_ty = Type::Array(Box::new(elem_ty));

                        self.fulfills(&expected, &arr_ty)?;

                        return Ok(arr_ty)
                    }
                }
            }
            Expr::Assign { 
                span, 
                assign_op:AssignOp::Assign, 
                target:MemberOrVar::Var { 
                    span:_, 
                    name, 
                    id, 
                    ty 
                }, 
                value 
            } => {
                if !self.variables.contains_key(&id){
                    return Err(Error::syntax_error(*span, format!("variable '{}' use before declare", name)))
                };

                self.trace_type(&value, Some(ty))?;

                return Ok(ty.clone())
            },
            Expr::Await { span, value } => {

                let ty = self.trace_type(&value, None)?;

                if let Type::Promise(p) = ty{
                    if let Some(expected) = expected{
                        self.fulfills(expected, &p)?;
                    }
                    return Ok(*p)
                } else{
                    if let Some(expected) = expected{
                        self.fulfills(expected, &ty)?;
                    }
                    return Ok(ty)
                }
            },
            Expr::BigInt(_) => {
                if let Some(expected) = &expected{
                    self.fulfills(expected, &Type::BigInt)?;
                }

                return Ok(Type::BigInt)
            }
            Expr::Bin { span, op, left, right } => {
                let left_ty = self.trace_type(&left, None)?;
                let right_ty = self.trace_type(&right, None)?;

                let re_ty =
                match op{
                    BinOp::Add
                    | BinOp::Div
                    | BinOp::Exp
                    | BinOp::Mod
                    | BinOp::Mul
                    | BinOp::Sub => {
                        if left_ty == Type::Int && right_ty == Type::Int{
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number{
                            Type::Number
                        } else if left_ty == Type::Number && right_ty == Type::Int{
                            Type::Number
                        } else if left_ty == Type::Int && right_ty == Type::Number{
                            Type::Number
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt{
                            Type::BigInt
                        } else if *op == BinOp::Add && left_ty == Type::String && right_ty == Type::String{
                            Type::String
                        } else{
                            return Err(Error::syntax_error(*span, format!("operation '{}' have incompatible types: {} and {}", op, left_ty, right_ty)))
                        }
                    },
                    BinOp::EqEq
                    | BinOp::EqEqEq
                    | BinOp::NotEq
                    | BinOp::NotEqEq => {
                        if left_ty != right_ty{
                            return Err(Error::syntax_error(*span, format!("operands of '{}' must be the same type, found {} and {}", op, left_ty, right_ty)))
                        }

                        Type::Bool
                    }
                    BinOp::And => Type::Bool,
                    BinOp::Nullish
                    | BinOp::Or => Type::union(left_ty, right_ty),
                    BinOp::BitAnd 
                    | BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::RShift
                    | BinOp::LShift
                    | BinOp::URShift => {
                        if left_ty == Type::Int && right_ty == Type::Int{
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number{
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Int{
                            Type::Int
                        } else if left_ty == Type::Int && right_ty == Type::Number{
                            Type::Int
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt{
                            Type::BigInt
                        } else{
                            return Err(Error::syntax_error(*span, format!("operation '{}' have incompatible types: {} and {}", op, left_ty, right_ty)))
                        }
                    }
                    BinOp::Gt
                    | BinOp::Gteq
                    | BinOp::Lt
                    | BinOp::Lteq => {
                        if left_ty == Type::Int && right_ty == Type::Int{
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Number{
                            Type::Int
                        } else if left_ty == Type::Number && right_ty == Type::Int{
                            Type::Int
                        } else if left_ty == Type::Int && right_ty == Type::Number{
                            Type::Int
                        } else if left_ty == Type::BigInt && right_ty == Type::BigInt{
                            Type::BigInt
                        } else{
                            return Err(Error::syntax_error(*span, format!("operation '{}' have incompatible types: {} and {}", op, left_ty, right_ty)))
                        }
                    }
                    BinOp::In => {
                        if left_ty != Type::String{
                            return Err(Error::syntax_error(*span, "left-hand side of 'in' operator must have string type"))
                        }
                        Type::Bool
                    }
                };

                if let Some(expected) = &expected{
                    self.fulfills(expected, &re_ty)?;
                }

                return Ok(re_ty)
            },
            Expr::Bool(_) => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Bool)?;
                }
                Ok(Type::Bool)
            },
            Expr::Call { 
                span, 
                callee: Callee::Expr(calee_expr), 
                is_optchain, 
                type_args, 
                args 
            } => {
                debug_assert!(type_args.is_empty());

                let callee_ty = self.trace_type(&calee_expr, None)?;

                return self.trace_call(*span, &callee_ty, args, *is_optchain, expected)
            }
            Expr::Call { 
                span, 
                callee: Callee::Function(func), 
                is_optchain:_, 
                type_args, 
                args 
            } => {
                debug_assert!(type_args.is_empty());

                // check param length
                if args.len() != func.ty.params.len(){
                    return Err(Error::syntax_error(*span, format!("function '{}' expected {} arguments, {} were given", Type::Function{ type_args: Box::new([]), func: func.ty.clone()}, func.ty.params.len(), args.len())))
                }

                // check this type
                self.fulfills(&func.ty.this_ty, &self.this_type)?;

                // check arguments
                for (i, arg) in args.iter().enumerate(){
                    let t = self.trace_type(arg, func.ty.params.get(i))?;
                };

                // check return type
                if let Some(expected) = expected{
                    self.fulfills(expected, &func.ty.return_ty)?;
                }

                // return result type
                return Ok(func.ty.return_ty.clone())
            }
            Expr::Call { 
                span, 
                callee:Callee::Member { 
                    span: member_span, 
                    obj, 
                    prop, 
                    is_optchain:_ 
                }, 
                is_optchain, 
                type_args, 
                args 
            } => {
                debug_assert!(type_args.is_empty());

                let obj_ty = self.trace_type(&obj, None)?;

                let callee_ty = self.trace_member(&obj_ty, prop)?;

                let old_this = core::mem::replace(&mut self.this_type, obj_ty);

                let return_ty = self.trace_call(*span, &callee_ty, args, *is_optchain, expected)?;

                self.this_type = old_this;

                return Ok(return_ty)
            }
            Expr::Cast { span, value, to_ty } => {
                let ty = self.trace_type(&value, None)?;

                self.fulfills(&to_ty, &ty)?;

                return Ok(ty)
            }
            Expr::ClassMember { span, class, prop, type_args, is_optchain } => {
                let ty =
                match class{
                    Type::Array(_) => {}
                    Type::Class { span, type_args, class } => {

                    }
                    Type::Enum(e) => {

                    }
                    Type::Function { type_args, func } => {

                    }
                    Type::Interface { span, type_args, interface } => {

                    }
                    Type::Iterator(i) => {

                    }
                    _ => todo!()
                };

                return Ok()
            }
            Expr::SuperMember { span, prop } => {

            }
            Expr::Function { span, type_args, func } => {
                debug_assert!(type_args.is_empty());

                let ty = Type::Function { 
                    type_args: Box::new([]), 
                    func: func.ty.clone()
                };
                if let Some(expected) = expected{
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty)
            }
            Expr::ImportMeta => todo!(),
            Expr::NewTarget => todo!(),
            Expr::InstanceOf { span, value, ty } => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Bool)?;
                }
                return Ok(Type::Bool)
            }
            Expr::Integer(_) => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Int)?;
                }
                Ok(Type::Int)
            },
            Expr::Member { span, obj, prop, type_args, is_optchain } => {
                debug_assert!(type_args.is_empty());

                let obj_ty = self.trace_type(&obj, None)?;

                let ty = self.trace_member(&obj_ty, prop)?;

                if let Some(expected) = expected{
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty)
            }
            Expr::New { span, callee, args } => {
                match callee{
                    Type::Class { span, type_args, class } => {
                        debug_assert!(type_args.is_empty());
                    }
                    _ => {
                        return Err(Error::syntax_error(*span, "new operator can only be called on a class"))
                    }
                };

                if let Some(expected) = expected{
                    self.fulfills(expected, callee)?;
                }
                return Ok(callee.clone())
            }
            Expr::Null => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Null)?;
                }
                Ok(Type::Null)
            },
            Expr::Number(_) => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Number)?;
                }
                Ok(Type::Number)
            },
            Expr::PrivateNameIn { span, name, value } => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Bool)?;
                }
                return Ok(Type::Bool)
            }
            Expr::ReadVar { span, name, id, ty } => {
                if !self.variables.contains_key(&id){
                    return Err(Error::syntax_error(*span, format!("variable '{}' use before declare", name)))
                };

                if let Some(expected) = expected{
                    self.fulfills(expected, ty)?;
                }
                return Ok(ty.clone())
            }
            Expr::Regex { .. } => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Regex)?;
                }
                Ok(Type::Regex)
            },
            Expr::Seq { span, exprs } => {
                let mut ty = None;

                for (i, e) in exprs.iter().enumerate(){
                    if i == exprs.len() -1{
                        ty = Some(self.trace_type(e, expected)?);
                    } else{
                        self.trace_type(e, None)?;
                    }
                }

                if let Some(ty) = ty{
                    if let Some(expected) = expected{
                        self.fulfills(expected, &ty)?;
                    }

                    return Ok(ty)
                } else{
                    return Ok(Type::Undefined)
                }
            },
            Expr::String(_) => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::String)?;
                }

                Ok(Type::String)
            },
            Expr::Ternary { span, test, left, right } => {
                let test_ty = self.trace_type(&test, None)?;
                let left_ty = self.trace_type(&left, None)?;
                let right_ty = self.trace_type(&right, None)?;

                let ty = left_ty.union(right_ty);

                if let Some(expected) = expected{
                    self.fulfills(expected, &ty)?;
                }

                return Ok(ty)
            }
            Expr::This(_) => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &self.this_type)?;
                }

                return Ok(self.this_type.clone())
            }
            Expr::Undefined => {
                if let Some(expected) = expected{
                    self.fulfills(expected, &Type::Undefined)?;
                }
                return Ok(Type::Undefined)
            }
            Expr::UnaryOp { span, op, value } => {
                todo!()
            }
            Expr::Update { span, target, op } => {
                todo!()
            }
            Expr::Yield { span, delegate, value } => {
                todo!()
            }
        }
    }
}