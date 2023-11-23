use crate::common::{ClassId, FunctionId, InterfaceId, VariableId};

use super::Expr;

pub enum Stmt {
    DeclareClass(ClassId),
    DeclareInterface(InterfaceId),
    DeclareFunction(FunctionId),

    DeclareGenericClass(ClassId),
    DeclareGenericInterface(InterfaceId),
    DeclareGenericFunction(FunctionId),

    DeclareVar(VariableId),
    DropVar(VariableId),

    Block {
        label: Option<String>,
        body: Vec<Stmt>,
    },
    If {
        test: Expr,
        body: Vec<Stmt>,
        else_: Vec<Stmt>,
    },
    Loop {
        label: Option<String>,
        body: Vec<Stmt>,
    },

    Break(Option<String>),
    Continue(Option<String>),

    Return(Expr),
}
