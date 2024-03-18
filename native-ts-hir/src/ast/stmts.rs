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

#[derive(Debug, Clone)]
pub enum Stmt {
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
        ty: Type,
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
        test: Box<Expr>,
    },
    /// end if
    EndIf,
    /// else, must be after end if
    Else,
    /// end else
    EndElse,
    /// match a value
    Switch(Box<Expr>),
    /// a switch case
    SwitchCase(Box<Expr>),
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
        update: Option<Box<Expr>>,
        end_check: Option<Box<Expr>>,
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

    Return(Box<Expr>),
    Throw(Box<Expr>),
    Expr(Box<Expr>),
}
