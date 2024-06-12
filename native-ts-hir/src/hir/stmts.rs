use std::fmt::Debug;

use native_ts_parser::swc_core::common::Span;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::common::{ClassId, FunctionId, InterfaceId, VariableId};

use super::{Expr, Type};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    Var,
    Let,
    Const,
    Using,
    AwaitUsing,
}

impl From<swc::VarDeclKind> for VarKind {
    fn from(value: swc::VarDeclKind) -> Self {
        match value {
            swc::VarDeclKind::Const => Self::Const,
            swc::VarDeclKind::Let => Self::Let,
            swc::VarDeclKind::Var => Self::Var,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<TY = Type>
where
    TY: Debug + Clone + From<Type>,
{
    /// declares a class type
    DeclareClass(ClassId),
    /// declares an interface type
    DeclareInterface(InterfaceId),
    /// declares a function
    DeclareFunction(FunctionId),
    /// declares a generic class
    DeclareGenericClass(ClassId),
    /// declares a generic interface
    DeclareGenericInterface(InterfaceId),
    /// declares a generic function
    DeclareGenericFunction(FunctionId),
    /// declares a variable
    DeclareVar {
        kind: VarKind,
        id: VariableId,
        ty: TY,
    },
    /// indicate variable is out of scope
    DropVar(VariableId),
    /// start of a block
    Block {
        label: String,
    },
    /// end of a block
    EndBlock,
    /// jump if condition
    If {
        test: Box<Expr<TY>>,
    },
    /// end if
    EndIf,
    /// else, must be after end if
    Else,
    /// end else
    EndElse,
    /// match a value
    Switch(Box<Expr<TY>>),
    /// a switch case
    SwitchCase(Box<Expr<TY>>),
    /// end of switch case
    EndSwitchCase,
    /// default case
    DefaultCase,
    /// end of default case
    EndDefaultCase,
    /// end of a switch
    EndSwitch,
    /// a loop
    Loop {
        label: Option<String>,
        update: Option<Box<Expr<TY>>>,
        end_check: Option<Box<Expr<TY>>>,
    },
    ForOfLoop {
        span: Span,
        label: Option<String>,
        binding: VariableId,
        target: Box<Expr<TY>>,
    },
    /// end of loop
    EndLoop,

    Try,
    EndTry,
    Catch(VariableId),
    EndCatch,
    Finally,
    EndFinally,

    /// break from a loop
    Break(Option<String>),
    ///
    Continue(Option<String>),

    Return(Box<Expr<TY>>),
    Throw(Box<Expr<TY>>),
    Expr(Box<Expr<TY>>),
}
