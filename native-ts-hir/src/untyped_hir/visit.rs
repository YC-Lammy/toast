
use std::rc::Rc;

use super::Callee;
use super::ClassType;
use super::Expr;
use super::Function;
use super::MemberOrVar;
use super::Stmt;
use super::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakOrContinue{
    Break,
    Continue,
}

pub trait Visitor{
    type Error;
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Result<BreakOrContinue, Self::Error>{
        Ok(BreakOrContinue::Continue)
    }
    fn visit_expr(&mut self, expr: &mut Expr) -> Result<BreakOrContinue, Self::Error>{
        Ok(BreakOrContinue::Continue)
    }
    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error>{
        Ok(BreakOrContinue::Break)
    }
    fn visit_class(&mut self, class: &mut Rc<ClassType>) -> Result<BreakOrContinue, Self::Error>{
        Ok(BreakOrContinue::Break)
    }
    fn visit_function(&mut self, function: &mut Rc<Function>) -> Result<BreakOrContinue, Self::Error>{
        Ok(BreakOrContinue::Break)
    }
}

pub trait Visit{
    fn visit<V:Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}


impl Visit for Expr{
    fn visit<V:Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error>{
        match self{
            Self::Array { span, values } => {
                for v in values{
                    v.visit(visitor)?;
                };
            }
            Self::Assign { span, assign_op, target, value } => {
                match target{
                    MemberOrVar::Member { span, obj, prop } => {
                        obj.visit(visitor)?
                    }
                    MemberOrVar::Var { span, name, id, ty  } => {
                        ty.visit(visitor)?
                    }
                };
                
                value.visit(visitor)?;

            },
            Self::Await { span, value } => {
                value.visit(visitor)?;
            }
            Self::BigInt(_) => {},
            Self::Bin { span, op, left, right } => {
                left.visit(visitor)?;
                right.visit(visitor)?;
            }
            Self::Bool(_) => {},
            Self::Call { span, callee, is_optchain, type_args, args } => {
                match callee{
                    Callee::ClassMember { span, class, prop, is_optchain } => {}
                    Callee::Expr(e) => {
                        e.visit(visitor)?;
                    }
                    Callee::Function(f) => {}
                    Callee::Member { span, obj, prop, is_optchain } => {
                        obj.visit(visitor)?;
                    }
                    Callee::Super => {}
                }
            }
            Self::Cast { span, value, to_ty } => {
                value.visit(visitor)?;
                to_ty.visit(visitor)?;
            }
            Self::ClassMember { span, class, prop, is_optchain } => {},
            Self::Enum { span, enum_ty, variant } => {},
            Self::Function(f) => {},
            Self::ImportMeta => {},
            Self::Integer(_) => {},
            Self::Member { span, obj, prop, is_optchain } => {
                obj.visit(visitor)?;
            }
            Self::New { span, callee, type_args, args } => {
                callee.visit(visitor)?;

                for a in type_args{
                    a.visit(visitor)?;
                }

                for arg in args{
                    arg.visit(visitor)?;
                }
            }
            Self::NewTarget => {},
            Self::Null => {},
            Self::Number(_) => {},
            Self::PrivateNameIn { span, name, value } => {
                value.visit(visitor)?;
            }
            Self::ReadVar { span, name, id, ty } => {
                ty.visit(visitor)?;
            }
            Self::Regex { reg, flags } => {},
            Self::Seq { span, exprs } => {
                for e in exprs{
                    e.visit(visitor)?;
                }
            }
            Self::String(_) => {},
            Self::SuperMember { span, prop } => {},
            Self::Ternary { span, test, left, right } => {
                test.visit(visitor)?;
                left.visit(visitor)?;
                right.visit(visitor)?;
            }
            Self::This(_) => {},
            Self::TypedFunction { span, type_args, func } => {
                for ty in type_args{
                    ty.visit(visitor)?;
                }
            }
            Self::UnaryOp { span, op, value } => {
                value.visit(visitor)?;
            }
            Self::Undefined => {},
            Self::Update { span, target, op } => {
                match target{
                    MemberOrVar::Member { span, obj, prop } => {
                        obj.visit(visitor)?;
                    }
                    MemberOrVar::Var { span, name, id, ty } => {
                        ty.visit(visitor)?;
                    }
                }
            }
            Self::Yield { span, delegate, value } => {
                value.visit(visitor)?;
            }
        };
        
        // call the visitor
        visitor.visit_expr(self)?;

        return Ok(())
    }
}

impl Visit for Type{
    fn visit<V:Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error>{
        
        match self{
            Self::Null
            | Self::Undefined
            | Self::Int
            | Self::Number
            | Self::BigInt
            | Self::String
            | Self::Regex
            | Self::Symbol
            | Self::Bool
            | Self::Any 
            | Self::Enum(_)
            | Self::Unknown{..}
            | Self::This
            | Self::Super
            | Self::Return 
            | Self::Generic(_) => {},
            Self::Array(t)
            | Self::Awaited(t)
            | Self::Iterator(t)
            | Self::Map(t)
            | Self::Promise(t) => {
                t.visit(visitor)?;
            }
            Self::Function(f) => {},
            Self::TypedFunction { type_args, func } => {
                for ty in type_args.iter_mut(){
                    ty.visit(visitor)?;
                }
            }
            Self::Union(u) => {
                for ty in u{
                    ty.visit(visitor)?;
                }
            }
            Self::Class(c) => {

            }
            Self::TypedClass { type_args, class } => {
                for ty in type_args.iter_mut(){
                    ty.visit(visitor)?;
                }
            }
            Self::Interface(i) => {

            }
            Self::TypedInterface { type_args, interface } => {
                for ty in type_args.iter_mut(){
                    ty.visit(visitor)?;
                }
            }
            Self::Alias(a) => {

            }
            Self::TypedAlias { type_args, alias } => {
                for ty in type_args.iter_mut(){
                    ty.visit(visitor)?;
                }
            }
        }
        
        // call the visitor
        visitor.visit_type(self)?;

        return Ok(())
    }
}