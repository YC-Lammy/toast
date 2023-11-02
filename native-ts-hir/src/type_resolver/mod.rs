use std::collections::HashMap;

use native_js_common::error::Error;
use swc_common::{Span, Spanned};

use crate::{untyped_hir::{UnknownId, Type, visit::{Visitor, BreakOrContinue}, Stmt, Expr, MemberOrVar, BinOp, UnaryOp, Callee, ClassType}, VarId, PropName};

pub struct UnknownFinder{
    resolved_unknown: HashMap<UnknownId, Type>,
}

impl UnknownFinder{
    fn try_resolve_unknown(&mut self, ty:&mut Type, init: Option<&mut Expr>){
        if let Type::Unknown { span, id } = ty{
            // unknown is already resolved
            if let Some(resolved) = self.resolved_unknown.get(id){
                *ty = resolved.clone();
                return;
            }

            if let Some(e) = init{
                // trey to trace the initialiser
                if let Some(resolved) = self.trace_expr(e){
                    self.resolved_unknown.insert(*id, resolved.clone());
                    *ty = resolved;
                    return;
                }
            };
        }
    }
    
    fn trace_expr(&mut self, expr: &mut Expr) -> Option<Type>{
        match expr{
            Expr::This(_) => Some(Type::This),
            Expr::Null => Some(Type::Null),
            Expr::Undefined => Some(Type::Undefined),
            Expr::Bool(_) => Some(Type::Bool),
            Expr::Integer(_) => Some(Type::Int),
            Expr::Number(_) => Some(Type::Number),
            Expr::BigInt(_) => Some(Type::BigInt),
            Expr::String(_) => Some(Type::String),
            Expr::Regex { .. } => Some(Type::Regex),
            Expr::Function(f) => Some(Type::Function(f.ty.clone())),
            Expr::TypedFunction { span, type_args, func } => {
                Some(Type::TypedFunction { 
                    type_args: type_args.as_slice().into(), 
                    func: func.ty.clone() 
                })
            }
            Expr::Enum { enum_ty, ..} => Some(Type::Enum(enum_ty.clone())),
            Expr::NewTarget => todo!(),
            Expr::ImportMeta => todo!(),
            Expr::Array { span, values } => {
                let mut tys = Vec::new();
                for e in values{
                    let t = self.trace_expr(e)?;
                    if !tys.contains(&t){
                        tys.push(t);
                    }
                };

                if tys.len() == 0{
                    return None
                }

                if tys.len() == 1{
                    return Some(Type::Array(Box::new(tys[0])))
                }

                return Some(Type::Array(Box::new(Type::Union(tys))))
            }
            Expr::Assign { target, .. } => {
                match target{
                    MemberOrVar::Var { ty, .. } => {
                        return Some(ty.clone())
                    }
                    MemberOrVar::Member { span, obj, prop } => {
                        return self.trace_member(obj, prop, false)
                    }
                }
            }
            Expr::Await { value, .. } => {
                let ty = self.trace_expr(value)?;
                return Some(ty.awaited())
            }
            
            Expr::Bin { op, left, right, .. } => {
                match op{
                    BinOp::Add
                    | BinOp::Mod
                    | BinOp::Mul
                    | BinOp::Sub => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;


                        if l == Type::Number && r == Type::Number{
                            return Some(Type::Number)
                        }
                        if l == Type::Int && r == Type::Int{
                            return Some(Type::Int)
                        }
                        if (l == Type::Number || l==Type::Int) && (r == Type::Number || r == Type::Int){
                            return Some(Type::Number)
                        }
                        
                        if *op == BinOp::Add{
                            if l == Type::String && r == Type::String{
                                return Some(Type::String)
                            }
                        }
                    },
                    | BinOp::Div
                    | BinOp::Exp => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;


                        if (l == Type::Number || l==Type::Int) && (r == Type::Number || r == Type::Int){
                            return Some(Type::Number)
                        }
                    }
                    BinOp::LShift
                    | BinOp::RShift
                    | BinOp::URShift 
                    | BinOp::BitAnd 
                    | BinOp::BitOr 
                    | BinOp::BitXor => {
                        return Some(Type::Int)
                    }
                    BinOp::And
                    | BinOp::EqEq
                    | BinOp::EqEqEq
                    | BinOp::NotEq
                    | BinOp::NotEqEq
                    | BinOp::Gt
                    | BinOp::Gteq
                    | BinOp::Lt
                    | BinOp::Lteq 
                    | BinOp::In
                    | BinOp::InstanceOf => {
                        return Some(Type::Bool)
                    }
                    BinOp::Or
                    | BinOp::Nullish => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;

                        return Some(l.union(r))
                    }
                };
                return None
            }
            Expr::UnaryOp { op, value, .. } => {
                match op{
                    UnaryOp::BitNot => Some(Type::Int),
                    UnaryOp::Delete => Some(Type::Bool),
                    UnaryOp::LogicalNot => Some(Type::Bool),
                    UnaryOp::Pos
                    | UnaryOp::Minus => {
                        let l = self.trace_expr(value)?;

                        if l == Type::Number{
                            return Some(Type::Number)
                        }
                        if l == Type::Int{
                            return Some(Type::Int)
                        }

                        return None
                    }
                    UnaryOp::Void => Some(Type::Undefined),
                    UnaryOp::Typeof => Some(Type::String)
                }
            }
            Expr::New { callee, type_args, .. } => {
                match callee{
                    Type::Class(c) => {
                        Some(Type::TypedClass { 
                            type_args: type_args.as_slice().into(), 
                            class: c.clone()
                        })
                    }
                    Type::Alias(a) => {
                        Some(Type::TypedAlias { 
                            type_args: type_args.as_slice().into(), 
                            alias: a.clone() 
                        })
                    }
                    _ => None
                }
            }
            Expr::Call { 
                span, 
                callee: Callee::Function(f), 
                is_optchain, 
                type_args, 
                ..
            } => {
                if f.ty.generics.is_empty(){
                    return Some(f.ty.return_ty.clone())
                }

                let called = Type::Called { 
                    span: *span,
                    is_optchain: *is_optchain,
                    type_args: type_args.as_slice().into(), 
                    func: Box::new(Type::Function(f.ty.clone()))
                };

                return Some(called)
            }
            Expr::Call {
                callee: Callee::Super, 
                is_optchain, 
                .. 
            } => {
                return Some(Type::Super)
            }
            Expr::Call{
                span,
                callee,
                is_optchain,
                type_args,
                ..
            } => {
                let mut callee_is_optchain = false;

                let f = match callee{
                    Callee::ClassMember { class, prop, is_optchain, .. } => {
                        callee_is_optchain = *is_optchain;
                        self.trace_class_member(class, prop, *is_optchain)?
                    }
                    Callee::Expr(e) => {
                        self.trace_expr(e)?
                    }
                    Callee::Member { span, obj, prop, is_optchain } => {
                        callee_is_optchain = *is_optchain;
                        self.trace_member(obj, prop, *is_optchain)?
                    }
                    _ => unreachable!()
                };

                match f{
                    Type::Function(f) => {
                        if f.generics.is_empty(){
                            return Some(f.return_ty.clone())
                        }
                    }
                    _ => {}
                }

                return Some(Type::Called { 
                    span: *span, 
                    is_optchain: *is_optchain, 
                    type_args: type_args.as_slice().into(), 
                    func: Box::new(f)
                })
            }
            Expr::SuperMember{..} => {
                todo!()
            }
            Expr::Member { obj, prop, is_optchain, .. } => {
                self.trace_member(obj, prop, *is_optchain)
            }
            Expr::ClassMember { class, prop, is_optchain, .. } => {
                self.trace_class_member(class, prop, *is_optchain)
            }
            Expr::ReadVar { ty, .. } => {
                self.try_resolve_unknown(ty, None);
                return Some(ty.clone())
            }
            Expr::Update { 
                target: MemberOrVar::Var { ty, .. },
                ..
            } => {
                return Some(ty.clone())
            }
            Expr::Update{
                target: MemberOrVar::Member{obj, prop, ..},
                ..
            } => {
                self.trace_member(obj, prop, false)
            }
            Expr::Yield { .. } => None,
            Expr::Ternary { left, right , .. } => {
                let l = self.trace_expr(left)?;
                let r = self.trace_expr(right)?;

                return Some(l.union(r))
            }
            Expr::Seq { exprs, .. } => {
                if let Some(last) = exprs.last_mut(){
                    return self.trace_expr(last)
                }
                return None
            }
            Expr::Cast { to_ty, .. } => {
                Some(to_ty.clone())
            }
            Expr::PrivateNameIn { .. } => Some(Type::Bool)
        }
    }

    fn trace_member(&mut self, obj: &mut Expr, prop: &PropName, optchain: bool) -> Option<Type>{
        let obj = self.trace_expr(obj)?;

        match obj{
            Type::Class(c) => {
                if let Some(attr) = c.attributes.iter().find(|attr|attr.name.eq(prop)){
                    return Some(attr.ty.clone())
                }

                if optchain{
                    return Some(Type::Undefined)
                }
            }
            Type::TypedClass { type_args, class } => {
                todo!()
            }
            Type::Interface(iface) => {
                if let Some(attr) = iface.props.iter().find(|attr|attr.name.eq(prop)){
                    if attr.optinal{
                        return Some(attr.ty.clone().optional())
                    }
                    return Some(attr.ty.clone())
                }

                if optchain{
                    return Some(Type::Undefined)
                }
            }
            Type::TypedInterface { type_args, interface } => {
                todo!()
            }
            Type::Alias(a) => {
                todo!()
            }
            Type::Array(a) => {
                if let PropName::Int(i) = prop{
                    if optchain{
                        return Some(a.optional())
                    }
                    return Some(*a)
                }

                if optchain{
                    return Some(Type::Undefined)
                }
            }
            _ => {}
        }

        return None;
    } 

    fn trace_class_member(&mut self, class_ty: &ClassType, prop: &PropName, optchain:bool) -> Option<Type>{
        if class_ty.generics.len() != 0{
            todo!()
        }

        if let Some(attr) = class_ty.static_props.iter().find(|a|a.name.eq(prop)){
            return Some(attr.ty.clone())
        }

        if let Some(m) = class_ty.static_functions.iter().find(|f|f.name.eq(prop)){
            return Some(Type::Function(m.function.ty.clone()))
        }

        if optchain{
            return Some(Type::Undefined)
        }

        return None;
    }
}

impl Visitor for UnknownFinder{
    type Error = Error<Span>;
    fn visit_stmt(&mut self, stmt: &mut crate::untyped_hir::Stmt) -> Result<BreakOrContinue, Self::Error> {
        match stmt{
            // variable declare may have unknown type
            Stmt::Declare { span, kind, name, id, ty, init } => {
                self.try_resolve_unknown(ty, init.as_mut());
            }
            _ => {}
        }
        return Ok(BreakOrContinue::Continue)
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<BreakOrContinue, Self::Error> {
        match expr{
            // variable assignment may be able to give us type
            Expr::Assign { 
                span:_, 
                assign_op, 
                target: MemberOrVar::Var { span, name, id, ty }, 
                mut value 
            } => {
                self.try_resolve_unknown(ty, Some(&mut value));
            }
            _ => {}
        }
        return Ok(BreakOrContinue::Continue)
    }
}

pub struct UnknownResolver{
    resolved_unknown: HashMap<UnknownId, Type>    
}

impl Visitor for UnknownResolver{
    type Error = Error<Span>;
    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        if let Type::Unknown { span, id } = ty{
            if let Some(resolved) = self.resolved_unknown.get(id){
                *ty = resolved.clone();
                return Ok(BreakOrContinue::Break)
            };
            return Err(Error::syntax_error(*span, "type annotation required"))
        }
        return Ok(BreakOrContinue::Continue)
    }
}