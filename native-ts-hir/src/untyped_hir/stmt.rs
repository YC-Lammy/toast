use std::sync::Arc;

use swc_common::Span;

use crate::{VarId, VarKind};

use super::{expr::Expr, Type, ClassType, Function};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt<TY=Type, CL= ClassType, F=Function>{
    Empty,
    /// declaration of a function
    Func(Arc<F>),
    /// declaration of a class
    Class(Arc<CL>),
    /// a block
    Block {
        span: Span,
        label: String,
    },
    EndBlock,
    /// a loop
    Loop {
        span: Span,
        label: Option<String>,
    },
    EndLoop,
    /// if else
    If {
        span: Span,
        test: Expr<TY, CL, F>,
    },
    EndIf,
    Else,
    EndElse,
    /// variable declare
    Declare {
        span: Span,
        kind: VarKind,
        name: String,
        id: VarId,
        ty: TY,
        init: Option<Expr<TY, CL, F>>,
    },
    /// return
    Return {
        span: Span,
        value: Expr<TY, CL, F>,
    },
    /// break, label is checked and valid
    Break {
        span: Span,
        /// must be valid
        label: Option<String>,
    },
    /// continue
    Continue {
        span: Span,
        /// must be valid
        label: Option<String>,
    },
    /// throw
    Throw {
        span: Span,
        value: Expr<TY, CL, F>,
        ty: TY,
    },
    /// switch
    Switch {
        span: Span,
        test: Expr<TY, CL, F>,
    },
    SwitchCase {
        span: Span,
        test: Option<Expr<TY, CL, F>>,
    },
    EndSwitchCase,
    EndSwitch,
    /// try
    Try {
        span: Span,
        stmts: Vec<Stmt<TY, CL, F>>,
    },
    Catch {
        span: Span,
        catch_binding: VarId,
        catch_binding_ty: TY,
        catch_binding_name: String,
    },
    Finally {
        span: Span,
    },
    EndTry,
    /// an expr stmt
    Expr {
        span: Span,
        expr: Expr<TY, CL, F>,
        ty: TY,
    },
}
