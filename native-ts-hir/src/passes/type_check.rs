use std::collections::HashMap;

use native_js_common::error::Error;
use swc_common::Span;

use crate::{VarId, untyped_hir::{Type, visit::{Visitor, BreakOrContinue}, Expr, AssignOp, MemberOrVar, BinOp, Callee}};



pub struct TypeChecker{
    variables: HashMap<VarId, Type>,
    this_type: Type,
    return_type: Type,
    acc_types: Vec<Type>,
}

impl TypeChecker{
    fn fulfills(&self, expected: &Type, ty: &Type) -> Result<(), Error<Span>>{
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
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::Call { 
                span, 
                callee: Callee::Expr(calee_expr), 
                is_optchain, 
                type_args, 
                args 
            } => {
                let callee_ty = self.trace_type(&calee_expr, None)?;

                
            }
        }
    }
}