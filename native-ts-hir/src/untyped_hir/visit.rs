use super::Callee;
use super::ClassType;
use super::Expr;
use super::Function;
use super::FunctionType;
use super::InterfaceType;
use super::MemberOrVar;
use super::Stmt;
use super::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakOrContinue {
    Break,
    Continue,
}

pub trait Visitor {
    type Error;
    const FINGER_PRINT: usize;
    const DEPTH_FIRST: bool = true;

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Result<BreakOrContinue, Self::Error> {
        let _ = stmt;
        Ok(BreakOrContinue::Continue)
    }
    fn visit_expr(&mut self, expr: &mut Expr) -> Result<BreakOrContinue, Self::Error> {
        let _ = expr;
        Ok(BreakOrContinue::Continue)
    }
    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        let _ = ty;
        Ok(BreakOrContinue::Break)
    }
    fn visit_class(&mut self, class: &mut ClassType) -> Result<BreakOrContinue, Self::Error> {
        let _ = class;
        Ok(BreakOrContinue::Break)
    }
    fn visit_function(
        &mut self,
        function: &mut Function,
    ) -> Result<BreakOrContinue, Self::Error> {
        let _ = function;
        Ok(BreakOrContinue::Break)
    }
}

pub trait Visit {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error>;
}

impl Visit for Stmt{
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        match self{
            Self::Block { .. } => {}
            Self::EndBlock => {}
            Self::Try { .. } => {}
            Self::Catch { catch_binding_ty, .. } => {
                catch_binding_ty.visit(visitor)?;
            }
            Self::Class(c) => {
                c.visit(visitor)?;
            }
            Self::Continue { .. } => {},
            Self::Break { .. } => {}
            Self::Declare { ty, init, .. } => {
                ty.visit(visitor)?;
                if let Some(init) = init{
                    init.visit(visitor)?;
                }
            }
            Self::Else => {}
            Self::Empty => {},
            Self::EndElse => {}
            Self::EndIf => {}
            Self::EndLoop => {}
            Self::EndSwitch => {}
            Self::EndSwitchCase => {}
            Self::EndTry => {}
            Self::EndCatch => {}
            Self::EndTryFinally => {}
            Self::Expr { expr, .. } => {
                expr.visit(visitor)?;
            }
            Self::Finally { .. } => {}
            Self::Func(f) => {
                f.visit(visitor)?;
            }
            Self::If { test, .. } => {
                test.visit(visitor)?;
            }
            Self::Loop { .. } => {}
            Self::Return { value, .. } => {
                value.visit(visitor)?;
            }
            Self::Switch { test, .. } => {
                test.visit(visitor)?;
            }
            Self::SwitchCase { test, .. } => {
                if let Some(test) = test{
                    test.visit(visitor)?;
                }
            }
            Self::Throw { value, ty, .. } => {
                value.visit(visitor)?;
                ty.visit(visitor)?;
            }
        }
        visitor.visit_stmt(self)?;

        return Ok(())
    }
}

impl Visit for Expr {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        match self {
            Self::Array { span:_, values } => {
                for v in values {
                    v.visit(visitor)?;
                }
            }
            Self::Assign {
                span:_,
                assign_op:_,
                target,
                value,
            } => {
                match target {
                    MemberOrVar::Member { obj, .. } => obj.visit(visitor)?,
                    MemberOrVar::Var { ty, .. } => ty.visit(visitor)?,
                    MemberOrVar::ClassMember { class, .. } => {
                        class.visit(visitor)?;
                    }
                };

                value.visit(visitor)?;
            }
            Self::Await { value, .. } => {
                value.visit(visitor)?;
            }
            Self::BigInt(_) => {}
            Self::Bin {
                left,
                right,
                ..
            } => {
                left.visit(visitor)?;
                right.visit(visitor)?;
            }
            Self::Bool(_) => {}
            Self::SuperCall { args, .. } => {
                for arg in args.iter_mut() {
                    arg.visit(visitor)?;
                }
            }
            Self::Call {
                callee,
                type_args,
                args,
                ..
            } => {
                for ty in type_args.iter_mut(){
                    ty.visit(visitor)?;
                }
                for e in args.iter_mut(){
                    e.visit(visitor)?;
                }
                match callee {
                    Callee::ClassMember {
                        class,
                        ..
                    } => {
                        class.visit(visitor)?;
                    }
                    Callee::Expr(e) => {
                        e.visit(visitor)?;
                    }
                    Callee::Function(f) => {
                        f.visit(visitor)?;
                    }
                    Callee::Member {
                        obj,
                        ..
                    } => {
                        obj.visit(visitor)?;
                    }
                    Callee::Super => {}
                }
            },
            Self::Cast { value, to_ty, .. } => {
                value.visit(visitor)?;
                to_ty.visit(visitor)?;
            }
            Self::ClassMember {
                class,
                type_args,
                ..
            } => {
                for ty in type_args{
                    ty.visit(visitor)?;
                }
                class.visit(visitor)?;
            }
            Self::Enum {
                enum_ty,
                ..
            } => {
                Type::Enum(enum_ty.clone()).visit(visitor)?;
            }
            Self::Function {
                type_args,
                func,
                ..
            } => {
                for ty in type_args {
                    ty.visit(visitor)?;
                }

                func.ty.visit(visitor)?;

                func.visit(visitor)?;
            }
            Self::ImportMeta => {}
            Self::Integer(_) => {}
            Self::InstanceOf { span:_, value, ty } => {
                value.visit(visitor)?;
                ty.visit(visitor)?;
            }
            Self::Member {
                obj,
                type_args,
                ..
            } => {
                for ty in type_args{
                    ty.visit(visitor)?;
                }
                obj.visit(visitor)?;
            }
            Self::New {
                callee, 
                args,
                ..
            } => {
                callee.visit(visitor)?;

                for arg in args {
                    arg.visit(visitor)?;
                }
            }
            Self::NewTarget => {}
            Self::Null => {}
            Self::Number(_) => {}
            Self::PrivateNameIn { value, .. } => {
                value.visit(visitor)?;
            }
            Self::ReadVar { ty, .. } => {
                ty.visit(visitor)?;
            }
            Self::Regex { .. } => {}
            Self::Seq { exprs, .. } => {
                for e in exprs {
                    e.visit(visitor)?;
                }
            }
            Self::String(_) => {}
            Self::SuperMember { .. } => {}
            Self::Ternary {
                test,
                left,
                right,
                ..
            } => {
                test.visit(visitor)?;
                left.visit(visitor)?;
                right.visit(visitor)?;
            }
            Self::This(_) => {}
            Self::UnaryOp { value, .. } => {
                value.visit(visitor)?;
            }
            Self::Undefined => {}
            Self::Update { target, .. } => match target {
                MemberOrVar::Member { obj, .. } => {
                    obj.visit(visitor)?;
                }
                MemberOrVar::Var { ty, .. } => {
                    ty.visit(visitor)?;
                }
                MemberOrVar::ClassMember { class, .. } => {
                    class.visit(visitor)?;
                }
            },
            Self::Yield {
                value,
                ..
            } => {
                value.visit(visitor)?;
            }
        };

        // call the visitor
        visitor.visit_expr(self)?;

        return Ok(());
    }
}

impl Visit for Function{
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        if self.visitor_fingerprint == V::FINGER_PRINT{
            return Ok(())
        };

        self.visitor_fingerprint = V::FINGER_PRINT;

        self.ty.visit(visitor)?;

        for s in &mut self.stmts{
            s.visit(visitor)?;
        };

        return Ok(())
    }
}

impl Visit for Type {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        match self {
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
            | Self::Unknown { .. }
            | Self::This
            | Self::Super
            | Self::Return
            | Self::Generic(_) => {}
            Self::Array(t)
            | Self::Iterator(t)
            | Self::Map(t)
            | Self::Promise(t) => {
                t.visit(visitor)?;
            }
            Self::Function { type_args, func } => {
                for ty in type_args.iter_mut() {
                    ty.visit(visitor)?;
                }
                func.visit(visitor)?;
            }
            Self::Union(u) => {
                for ty in u {
                    ty.visit(visitor)?;
                }
            }
            Self::Class { span: _, type_args, class } => {
                for ty in type_args.iter_mut() {
                    ty.visit(visitor)?;
                }
                class.visit(visitor)?;
            }
            Self::Interface {
                span:_,
                type_args,
                interface,
            } => {
                for ty in type_args.iter_mut() {
                    ty.visit(visitor)?;
                }

                interface.visit(visitor)?;
            }
            Self::Alias {
                type_args, alias, ..
            } => {
                for ty in type_args.iter_mut() {
                    ty.visit(visitor)?;
                }
                alias.base.visit(visitor)?;
            }
        }

        // call the visitor
        visitor.visit_type(self)?;

        return Ok(());
    }
}

impl Visit for ClassType {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        // already visited
        if self.visit_fingerprint == V::FINGER_PRINT {
            return Ok(());
        }

        self.visit_fingerprint = V::FINGER_PRINT;

        for g in &mut self.generics{
            if let Some(d) = &mut g.default{
                d.visit(visitor)?;
            }
            if let Some(c) = &mut g.constrain{
                c.visit(visitor)?;
            }
        }

        if let Some(super_ty) = &mut self.extends{
            super_ty.visit(visitor)?;
        }

        for i in &mut self.implements{
            i.visit(visitor)?;
        }

        for attr in &mut self.attributes {
            attr.ty.visit(visitor)?;
        }

        for prop in &mut self.static_props {
            prop.ty.visit(visitor)?;
        }

        for m in &mut self.static_functions{
            m.function.ty.visit(visitor)?;
            visitor.visit_function(&mut m.function)?;
        }

        for m in &mut self.methods{
            m.function.ty.visit(visitor)?;
            visitor.visit_function(&mut m.function)?;
        }

        return Ok(());
    }
}

impl Visit for FunctionType {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        // already visited
        if self.visit_fingerprint == V::FINGER_PRINT {
            return Ok(());
        }

        self.visit_fingerprint = V::FINGER_PRINT;

        for p in &mut self.params {
            p.visit(visitor)?;
        }

        for g in &mut self.generics{
            if let Some(d) = &mut g.default{
                d.visit(visitor)?;
            }

            if let Some(c) = &mut g.constrain{
                c.visit(visitor)?;
            }
        }

        self.this_ty.visit(visitor)?;

        self.return_ty.visit(visitor)?;

        return Ok(());
    }
}

impl Visit for InterfaceType {
    fn visit<V: Visitor>(&mut self, visitor: &mut V) -> Result<(), V::Error> {
        if self.visited_fingerprint == V::FINGER_PRINT {
            return Ok(());
        }

        self.visited_fingerprint = V::FINGER_PRINT;

        for t in &mut self.extends {
            t.visit(visitor)?;
        }

        for p in &mut self.props {
            p.ty.visit(visitor)?;
        }

        for m in &mut self.methods {
            m.ty.visit(visitor)?;
        }

        for g in &mut self.generics{
            if let Some(d) = &mut g.default{
                d.visit(visitor)?;
            }
            if let Some(c) = &mut g.constrain{
                c.visit(visitor)?;
            }
        }

        return Ok(());
    }
}
