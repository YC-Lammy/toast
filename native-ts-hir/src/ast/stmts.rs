use crate::common::{ClassId, FunctionId, InterfaceId, VariableId};

use super::{Expr, Type};

pub enum Stmt {
    DeclareClass(ClassId),
    DeclareInterface(InterfaceId),
    DeclareFunction(FunctionId),

    DeclareGenericClass(ClassId),
    DeclareGenericInterface(InterfaceId),
    DeclareGenericFunction(FunctionId),

    DeclareVar(VariableId, Type),
    DropVar(VariableId),

    Block { label: String },
    EndBlock,
    If { test: Expr },
    EndIf,
    Else,
    EndElse,

    Switch(Expr),
    SwitchCase(Expr),
    EndSwitchCase,
    DefaultCase,
    EndDefaultCase,
    EndSwitch,

    Loop { label: Option<String> },
    EndLoop,

    Try,
    EndTry,
    Catch(VariableId, Type),
    EndCatch,
    Finally,
    EndFinally,

    Break(Option<String>),
    Continue(Option<String>),

    Return(Expr),
    Throw(Expr),
    Expr(Expr),
}
