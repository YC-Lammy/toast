use std::collections::HashMap;

use native_js_common::error::Error;
use swc_common::Span;

use crate::{
    untyped_hir::{
        visit::{BreakOrContinue, Visitor},
        BinOp, Callee, Expr, MemberOrVar, Stmt, Type, UnaryOp, UnknownId,
    },
    PropName,
};

pub struct UnknownFinder {
    resolved_unknown: HashMap<UnknownId, Type>,
}

impl UnknownFinder {
    fn try_resolve_unknown(&mut self, ty: &mut Type, init: Option<&mut Expr>) {
        if let Type::Unknown { span:_, id } = ty {
            // unknown is already resolved
            if let Some(resolved) = self.resolved_unknown.get(id) {
                // replace the original type definition
                *ty = resolved.clone();
                return;
            }
            if let Some(e) = init {
                // trey to trace the initialiser
                if let Some(resolved) = self.trace_expr(e) {
                    // save type to resolved unknowns
                    self.resolved_unknown.insert(*id, resolved.clone());
                    // replace to original type definition
                    *ty = resolved;
                    return;
                }
            };
        }
    }

    fn trace_expr(&mut self, expr: &mut Expr) -> Option<Type> {
        match expr {
            Expr::This(_) => Some(Type::This),
            Expr::Null => Some(Type::Null),
            Expr::Undefined => Some(Type::Undefined),
            Expr::Bool(_) => Some(Type::Bool),
            Expr::Integer(_) => Some(Type::Int),
            Expr::Number(_) => Some(Type::Number),
            Expr::BigInt(_) => Some(Type::BigInt),
            Expr::String(_) => Some(Type::String),
            Expr::Regex { .. } => Some(Type::Regex),
            Expr::Function {
                span:_,
                type_args,
                func,
            } => Some(Type::Function {
                type_args: type_args.as_slice().into(),
                func: func.ty.clone(),
            }),
            Expr::Enum { enum_ty, .. } => Some(Type::Enum(enum_ty.clone())),
            Expr::NewTarget => todo!(),
            Expr::ImportMeta => todo!(),
            Expr::Array { span:_, values } => {
                let mut tys = Vec::new();
                for e in values {
                    let t = self.trace_expr(e)?;
                    if !tys.contains(&t) {
                        tys.push(t);
                    }
                }

                if tys.len() == 0 {
                    return None;
                }

                if tys.len() == 1 {
                    return Some(Type::Array(Box::new(tys[0].clone())));
                }

                return Some(Type::Array(Box::new(Type::Union(tys))));
            }
            Expr::Assign { target, .. } => match target {
                MemberOrVar::Var { ty, .. } => return Some(ty.clone()),
                MemberOrVar::Member { span:_, obj, prop } => {
                    return self.trace_member(obj, prop, false)
                }
                MemberOrVar::ClassMember { class, prop, .. } => {
                    return self.trace_class_member(class, prop, false)
                }
            },
            Expr::Await { value, .. } => {
                let ty = self.trace_expr(value)?;
                
                if let Type::Unknown { .. } = ty{
                    return None
                }

                if let Type::Promise(t) = ty{
                    return Some(*t)
                }

                return Some(ty)
            }

            Expr::Bin {
                op, left, right, ..
            } => {
                match op {
                    BinOp::Add | BinOp::Mod | BinOp::Mul | BinOp::Sub => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;

                        if l == Type::Number && r == Type::Number {
                            return Some(Type::Number);
                        }
                        if l == Type::Int && r == Type::Int {
                            return Some(Type::Int);
                        }
                        if (l == Type::Number || l == Type::Int)
                            && (r == Type::Number || r == Type::Int)
                        {
                            return Some(Type::Number);
                        }

                        if *op == BinOp::Add {
                            if l == Type::String && r == Type::String {
                                return Some(Type::String);
                            }
                        }
                    }
                    BinOp::Div | BinOp::Exp => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;

                        if (l == Type::Number || l == Type::Int)
                            && (r == Type::Number || r == Type::Int)
                        {
                            return Some(Type::Number);
                        }
                    }
                    BinOp::LShift
                    | BinOp::RShift
                    | BinOp::URShift
                    | BinOp::BitAnd
                    | BinOp::BitOr
                    | BinOp::BitXor => return Some(Type::Int),
                    BinOp::And
                    | BinOp::EqEq
                    | BinOp::EqEqEq
                    | BinOp::NotEq
                    | BinOp::NotEqEq
                    | BinOp::Gt
                    | BinOp::Gteq
                    | BinOp::Lt
                    | BinOp::Lteq
                    | BinOp::In => {
                        return Some(Type::Bool)
                    }
                    BinOp::Or | BinOp::Nullish => {
                        let l = self.trace_expr(left)?;
                        let r = self.trace_expr(right)?;

                        return Some(l.union(r));
                    }
                };
                return None;
            }
            Expr::UnaryOp { op, value, .. } => match op {
                UnaryOp::BitNot => Some(Type::Int),
                UnaryOp::Delete => Some(Type::Bool),
                UnaryOp::LogicalNot => Some(Type::Bool),
                UnaryOp::Plus | UnaryOp::Minus => {
                    let l = self.trace_expr(value)?;

                    if l == Type::Number {
                        return Some(Type::Number);
                    }
                    if l == Type::Int {
                        return Some(Type::Int);
                    }

                    return None;
                }
                UnaryOp::Void => Some(Type::Undefined),
                UnaryOp::Typeof => Some(Type::String),
            },
            Expr::New { callee, .. } => return Some(callee.clone()),
            Expr::SuperCall { .. } => return Some(Type::Super),
            Expr::Call {
                span:_,
                callee: Callee::Function(f),
                is_optchain:_,
                type_args,
                ..
            } => {

                debug_assert!(type_args.is_empty());
                debug_assert!(f.ty.generics.is_empty());

                return Some(f.ty.return_ty.clone());
            }
            Expr::Call {
                callee: Callee::Super,
                ..
            } => return Some(Type::Super),
            Expr::Call {
                span:_,
                callee,
                is_optchain:_,
                type_args,
                ..
            } => {
                // should be resolved
                debug_assert!(type_args.is_empty());

                let f = match callee {
                    Callee::ClassMember {
                        class,
                        prop,
                        is_optchain,
                        ..
                    } => {
                        self.trace_class_member(class, prop, *is_optchain)?
                    }
                    Callee::Expr(e) => self.trace_expr(e)?,
                    Callee::Member {
                        span: _,
                        obj,
                        prop,
                        is_optchain,
                    } => {
                        self.trace_member(obj, prop, *is_optchain)?
                    }
                    _ => unreachable!(),
                };

                match &f {
                    Type::Function { type_args, func } => {
                        // should be resolved
                        debug_assert!(type_args.is_empty());

                        return Some(func.return_ty.clone());
                        
                    }
                    _ => None
                }
            }
            Expr::SuperMember { .. } => {
                todo!()
            }
            Expr::Member {
                obj,
                prop,
                is_optchain,
                ..
            } => self.trace_member(obj, prop, *is_optchain),
            Expr::ClassMember {
                class,
                prop,
                is_optchain,
                ..
            } => self.trace_class_member(class, prop, *is_optchain),
            Expr::ReadVar { ty, .. } => {
                self.try_resolve_unknown(ty, None);
                return Some(ty.clone());
            }
            Expr::Update {
                target: MemberOrVar::Var { ty, .. },
                ..
            } => return Some(ty.clone()),
            Expr::Update {
                target: MemberOrVar::Member { obj, prop, .. },
                ..
            } => self.trace_member(obj, prop, false),
            Expr::Update {
                target:MemberOrVar::ClassMember { class, prop, .. },
                ..
            } => self.trace_class_member(class, prop, false),
            Expr::Yield { .. } => None,
            Expr::Ternary { left, right, .. } => {
                let l = self.trace_expr(left)?;
                let r = self.trace_expr(right)?;

                return Some(l.union(r));
            }
            Expr::Seq { exprs, .. } => {
                if let Some(last) = exprs.last_mut() {
                    return self.trace_expr(last);
                }
                return None;
            }
            Expr::InstanceOf { .. } => Some(Type::Bool),
            Expr::Cast { to_ty, .. } => Some(to_ty.clone()),
            Expr::PrivateNameIn { .. } => Some(Type::Bool),
        }
    }

    fn trace_member(&mut self, obj: &mut Expr, prop: &PropName, optchain: bool) -> Option<Type> {
        let obj = self.trace_expr(obj)?;

        match obj {
            Type::Class { span:_, type_args, class } => {
                // should be resolved
                debug_assert!(type_args.is_empty());
                debug_assert!(class.generics.is_empty());

                if let Some(attr) = class.attributes.iter().find(|attr| attr.name.eq(prop)) {
                    return Some(attr.ty.clone());
                }

                if let Some(m) = class.methods.iter().find(|m|m.name.eq(prop)){
                    debug_assert!(m.function.ty.generics.is_empty());

                    return Some(Type::Function { 
                        type_args: Box::new([]), 
                        func: m.function.ty.clone() 
                    })
                }

                if optchain {
                    return Some(Type::Undefined);
                }
            }
            Type::Interface {
                span:_,
                type_args,
                interface,
            } => {
                // shoould be resolved
                debug_assert!(type_args.is_empty());
                debug_assert!(interface.generics.is_empty());

                if let Some(attr) = interface.props.iter().find(|attr| attr.name.eq(prop)) {
                    if attr.optinal {
                        return Some(attr.ty.clone().optional());
                    }
                    return Some(attr.ty.clone());
                }

                if let Some(m) = interface.methods.iter().find(|m|m.name.eq(prop)){
                    return Some(Type::Function{
                        type_args: Box::new([]),
                        func: m.ty.clone()
                    })
                }

                if optchain {
                    return Some(Type::Undefined);
                }
            }
            Type::Alias { .. } => {
                // type aliases should be resolved at this point
                unreachable!()
            }
            Type::Array(a) => {
                if let PropName::Int(_) = prop {
                    if optchain {
                        return Some(a.optional());
                    }
                    return Some(*a);
                }

                if optchain {
                    return Some(Type::Undefined);
                }
            }
            _ => {}
        }

        return None;
    }

    fn trace_class_member(
        &mut self,
        class_ty: &Type,
        prop: &PropName,
        optchain: bool,
    ) -> Option<Type> {
        match class_ty{
            Type::Class { span:_, type_args, class } => {
                debug_assert!(type_args.is_empty());

                if let Some(p) = class.static_props.iter().find(|p|p.name.eq(prop)){
                    return Some(p.ty.clone())
                };

                if optchain{
                    return Some(Type::Undefined)
                }

                return None
            }
            Type::Enum(e) => {
                if let Some(_) = e.variants.iter().find(|e|e.name.eq(prop)){
                    return Some(Type::Enum(e.clone()))
                }
            }
            _ => {}
        }
        return None

    }
}

impl Visitor for UnknownFinder {
    type Error = Error<Span>;
    const FINGER_PRINT: usize = 1;

    fn visit_stmt(
        &mut self,
        stmt: &mut crate::untyped_hir::Stmt,
    ) -> Result<BreakOrContinue, Self::Error> {
        match stmt {
            // variable declare may have unknown type
            Stmt::Declare { ty, init, .. } => {
                self.try_resolve_unknown(ty, init.as_mut());
            }
            _ => {}
        }
        return Ok(BreakOrContinue::Continue);
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<BreakOrContinue, Self::Error> {
        match expr {
            // variable assignment may be able to give us type
            Expr::Assign {
                span: _,
                assign_op:_,
                target: MemberOrVar::Var { ty, .. },
                value,
            } => {
                self.try_resolve_unknown(ty, Some(value));
            }
            _ => {}
        }
        return Ok(BreakOrContinue::Continue);
    }
}

pub struct UnknownResolver {
    resolved_unknown: HashMap<UnknownId, Type>,
}

impl Visitor for UnknownResolver {
    type Error = Error<Span>;
    const FINGER_PRINT: usize = 2;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        // only replace unknowns
        if let Type::Unknown { span, id } = ty {
            // type found
            if let Some(resolved) = self.resolved_unknown.get(id) {
                *ty = resolved.clone();
                return Ok(BreakOrContinue::Break);
            };
            // cannot find type, tell user to annotate
            return Err(Error::syntax_error(*span, "type annotation required"));
        }
        return Ok(BreakOrContinue::Continue);
    }
}
