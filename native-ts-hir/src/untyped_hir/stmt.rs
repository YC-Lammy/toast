
use native_js_common::rc::Rc;
use swc_common::Span;

use crate::{VarId, VarKind};

use super::{expr::Expr, ClassType, Function, Type, DeepClone};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt<TY = Type, CL = ClassType, F = Function> {
    Empty,
    /// declaration of a function
    Func(Rc<F>),
    /// declaration of a class
    Class(Rc<CL>),
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
        test: Expr<TY, F>,
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
        init: Option<Expr<TY, F>>,
    },
    /// return
    Return {
        span: Span,
        value: Expr<TY, F>,
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
        value: Expr<TY, F>,
        ty: TY,
    },
    /// switch
    Switch {
        span: Span,
        test: Expr<TY, F>,
    },
    SwitchCase {
        span: Span,
        test: Option<Expr<TY, F>>,
    },
    EndSwitchCase,
    EndSwitch,
    /// try
    Try {
        span: Span
    },
    EndTry,
    Catch {
        span: Span,
        catch_binding: VarId,
        catch_binding_ty: TY,
        catch_binding_name: String,
    },
    EndCatch,
    Finally {
        span: Span,
    },
    EndTryFinally,
    /// an expr stmt
    Expr {
        span: Span,
        expr: Expr<TY, F>
    },
}


impl<TY, CL, F> DeepClone for Stmt<TY, CL, F> where TY:DeepClone, CL:DeepClone, F:DeepClone{
    fn deep_clone(&self) -> Self {
        match self{
            Self::Expr { span, expr } => Self::Expr { span: span.clone(), expr: expr.deep_clone() },
            Self::Catch { span, catch_binding, catch_binding_ty, catch_binding_name } => {
                Self::Catch { 
                    span: span.clone(), 
                    catch_binding: *catch_binding, 
                    catch_binding_ty: catch_binding_ty.deep_clone(), 
                    catch_binding_name: catch_binding_name.clone() 
                }
            }
            Self::Try { span } => {
                Self::Try { 
                    span: span.clone()
                }
            }
            _ => todo!()
        }
    }
}