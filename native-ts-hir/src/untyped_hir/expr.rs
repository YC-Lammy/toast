use std::fmt::Display;

use native_js_common::rc::Rc;

use crate::VarId;

use super::{types::*, EnumType};
use super::{DeepClone, Function};

pub use crate::PropName;
pub use crate::Symbol;

use swc_common::Span;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Callee<TY = Type, F = Function> {
    Super,
    Function(Rc<F>),
    ClassMember {
        span: Span,
        /// class or alias
        class: TY,
        prop: PropName,
        /// only valid in optchain call
        is_optchain: bool,
    },
    Member {
        span: Span,
        obj: Box<Expr<TY, F>>,
        prop: PropName,
        /// only valid in optchain call
        is_optchain: bool,
    },
    Expr(Box<Expr<TY, F>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    EqEq,
    EqEqEq,
    NotEq,
    NotEqEq,
    Lt,
    Lteq,
    Gt,
    Gteq,
    RShift,
    URShift,
    LShift,
    And,
    Or,
    BitOr,
    BitXor,
    BitAnd,
    Nullish,
    In,
}

impl From<swc_ecmascript::ast::BinaryOp> for BinOp {
    fn from(value: swc_ecmascript::ast::BinaryOp) -> Self {
        match value {
            swc_ecmascript::ast::BinaryOp::Add => Self::Add,
            swc_ecmascript::ast::BinaryOp::BitAnd => Self::BitAnd,
            swc_ecmascript::ast::BinaryOp::BitOr => Self::BitOr,
            swc_ecmascript::ast::BinaryOp::BitXor => Self::BitXor,
            swc_ecmascript::ast::BinaryOp::Div => Self::Div,
            swc_ecmascript::ast::BinaryOp::EqEq => Self::EqEq,
            swc_ecmascript::ast::BinaryOp::EqEqEq => Self::EqEqEq,
            swc_ecmascript::ast::BinaryOp::Exp => Self::Exp,
            swc_ecmascript::ast::BinaryOp::Gt => Self::Gt,
            swc_ecmascript::ast::BinaryOp::GtEq => Self::Gteq,
            swc_ecmascript::ast::BinaryOp::In => Self::In,
            swc_ecmascript::ast::BinaryOp::InstanceOf => unreachable!(),
            swc_ecmascript::ast::BinaryOp::LShift => Self::LShift,
            swc_ecmascript::ast::BinaryOp::LogicalAnd => Self::And,
            swc_ecmascript::ast::BinaryOp::LogicalOr => Self::Or,
            swc_ecmascript::ast::BinaryOp::Lt => Self::Lt,
            swc_ecmascript::ast::BinaryOp::LtEq => Self::Lteq,
            swc_ecmascript::ast::BinaryOp::Mod => Self::Mod,
            swc_ecmascript::ast::BinaryOp::Mul => Self::Mul,
            swc_ecmascript::ast::BinaryOp::NotEq => Self::NotEq,
            swc_ecmascript::ast::BinaryOp::NotEqEq => Self::NotEqEq,
            swc_ecmascript::ast::BinaryOp::NullishCoalescing => Self::Nullish,
            swc_ecmascript::ast::BinaryOp::RShift => Self::RShift,
            swc_ecmascript::ast::BinaryOp::Sub => Self::Sub,
            swc_ecmascript::ast::BinaryOp::ZeroFillRShift => Self::URShift,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Add => "+",
            Self::And => "&&",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Div => "/",
            Self::EqEq => "==",
            Self::EqEqEq => "===",
            Self::Exp => "**",
            Self::Gt => ">",
            Self::Gteq => ">=",
            Self::In => "in",
            Self::LShift => "<<",
            Self::Lt => "<",
            Self::Lteq => "<=",
            Self::Mod => "%",
            Self::Mul => "*",
            Self::NotEq => "!=",
            Self::NotEqEq => "!==",
            Self::Nullish => "??",
            Self::Or => "||",
            Self::RShift => ">>",
            Self::Sub => "-",
            Self::URShift => ">>>",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AssignOp {
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    ModAssign,
    /// `<<=`
    LShiftAssign,
    /// `>>=`
    RShiftAssign,
    /// `>>>=`
    ZeroFillRShiftAssign,
    /// `|=`
    BitOrAssign,
    /// `^=`
    BitXorAssign,
    /// `&=`
    BitAndAssign,

    /// `**=`
    ExpAssign,

    /// `&&=`
    AndAssign,

    /// `||=`
    OrAssign,

    /// `??=`
    NullishAssign,
}

impl From<swc_ecmascript::ast::AssignOp> for AssignOp {
    fn from(value: swc_ecmascript::ast::AssignOp) -> Self {
        match value {
            swc_ecmascript::ast::AssignOp::AddAssign => Self::AddAssign,
            swc_ecmascript::ast::AssignOp::AndAssign => Self::AndAssign,
            swc_ecmascript::ast::AssignOp::BitAndAssign => Self::BitAndAssign,
            swc_ecmascript::ast::AssignOp::BitOrAssign => Self::BitOrAssign,
            swc_ecmascript::ast::AssignOp::BitXorAssign => Self::BitXorAssign,
            swc_ecmascript::ast::AssignOp::DivAssign => Self::DivAssign,
            swc_ecmascript::ast::AssignOp::ExpAssign => Self::ExpAssign,
            swc_ecmascript::ast::AssignOp::LShiftAssign => Self::LShiftAssign,
            swc_ecmascript::ast::AssignOp::ModAssign => Self::ModAssign,
            swc_ecmascript::ast::AssignOp::MulAssign => Self::MulAssign,
            swc_ecmascript::ast::AssignOp::NullishAssign => Self::NullishAssign,
            swc_ecmascript::ast::AssignOp::RShiftAssign => Self::RShiftAssign,
            swc_ecmascript::ast::AssignOp::SubAssign => Self::SubAssign,
            swc_ecmascript::ast::AssignOp::ZeroFillRShiftAssign => Self::ZeroFillRShiftAssign,
            swc_ecmascript::ast::AssignOp::Assign => Self::Assign,
            swc_ecmascript::ast::AssignOp::OrAssign => Self::OrAssign,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MemberOrVar<TY, F> {
    Var {
        span: Span,
        name: String,
        id: VarId,
        ty: TY,
    },
    Member {
        span: Span,
        obj: Box<Expr<TY, F>>,
        prop: PropName,
    },
    ClassMember {
        span: Span,
        class: TY,
        prop: PropName,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UpdateOp {
    /// ++expr
    PrefixAdd,
    /// --expr
    PrefixSub,
    /// expr++
    SuffixAdd,
    /// expr--
    SuffixSub,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    LogicalNot,
    BitNot,
    Delete,
    Void,
    Typeof,
    Minus,
    Plus,
}

impl From<swc_ecmascript::ast::UnaryOp> for UnaryOp {
    fn from(value: swc_ecmascript::ast::UnaryOp) -> Self {
        match value {
            swc_ecmascript::ast::UnaryOp::Bang => Self::LogicalNot,
            swc_ecmascript::ast::UnaryOp::Delete => Self::Delete,
            swc_ecmascript::ast::UnaryOp::Minus => Self::Minus,
            swc_ecmascript::ast::UnaryOp::Plus => Self::Plus,
            swc_ecmascript::ast::UnaryOp::Tilde => Self::BitNot,
            swc_ecmascript::ast::UnaryOp::TypeOf => Self::Typeof,
            swc_ecmascript::ast::UnaryOp::Void => Self::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr<TY = Type, F = Function> {
    /// read this
    This(Span),
    /// null
    Null,
    /// undefined
    Undefined,
    /// boolean
    Bool(bool),
    /// int for faster operation, compatible with number
    Integer(i32),
    /// number
    Number(f64),
    /// bigint
    BigInt(i128),
    /// literal string
    String(String),
    /// regexp
    Regex {
        reg: String,
        flags: String,
    },
    Function {
        span: Span,
        type_args: Vec<TY>,
        func: Rc<F>,
    },

    /// new.target
    NewTarget,
    /// import.meta
    ImportMeta,
    Array {
        span: Span,
        values: Vec<Expr<TY, F>>,
    },
    /// expr + expr
    Bin {
        span: Span,
        op: BinOp,
        left: Box<Expr<TY, F>>,
        right: Box<Expr<TY, F>>,
    },
    /// ++ expr
    UnaryOp {
        span: Span,
        op: UnaryOp,
        value: Box<Expr<TY, F>>,
    },

    /// new Class()
    New {
        span: Span,
        /// class or alias
        callee: TY,
        //type_args: Vec<TY>,
        args: Vec<Expr<TY, F>>,
    },
    /// call operation
    Call {
        span: Span,
        callee: Callee<TY, F>,
        is_optchain: bool,
        type_args: Vec<TY>,
        args: Vec<Expr<TY, F>>,
    },

    /// super.prop, only valid within class methods
    SuperMember {
        span: Span,
        prop: PropName,
    },
    /// obj.prop
    Member {
        span: Span,
        obj: Box<Expr<TY, F>>,
        prop: PropName,
        /// if type args is not empty, prop must be a method
        type_args: Vec<TY>,
        is_optchain: bool,
    },
    // class static member
    ClassMember {
        span: Span,
        // alias or class
        class: TY,
        prop: PropName,
        /// if type args is some, prop must be a method
        type_args: Vec<TY>,
        is_optchain: bool,
    },
    Assign {
        span: Span,
        assign_op: AssignOp,
        target: MemberOrVar<TY, F>,
        value: Box<Expr<TY, F>>,
    },
    /// access a variable
    ReadVar {
        span: Span,
        name: String,
        id: VarId,
        ty: TY,
    },
    Update {
        span: Span,
        target: MemberOrVar<TY, F>,
        op: UpdateOp,
    },

    Await {
        span: Span,
        value: Box<Expr<TY, F>>,
    },
    Yield {
        span: Span,
        delegate: bool,
        value: Box<Expr<TY, F>>,
    },
    Ternary {
        span: Span,
        test: Box<Expr<TY, F>>,
        left: Box<Expr<TY, F>>,
        right: Box<Expr<TY, F>>,
    },
    Seq {
        span: Span,
        exprs: Vec<Expr<TY, F>>,
    },
    InstanceOf {
        span: Span,
        value: Box<Expr<TY, F>>,
        ty: TY,
    },
    Cast {
        span: Span,
        value: Box<Expr<TY, F>>,
        to_ty: TY,
    },
    /// this should be removed after type check
    PrivateNameIn {
        span: Span,
        name: String,
        value: Box<Expr<TY, F>>,
    },
}

impl<TY, F> Expr<TY, F> {}

impl<TY, F> DeepClone for Expr<TY, F>
where
    TY: DeepClone,
    F: DeepClone,
{
    fn deep_clone(&self) -> Self {
        match self {
            _ => todo!(),
        }
    }
}
