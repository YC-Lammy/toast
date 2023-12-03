use swc_common::Span;

use crate::common::{ClassId, FunctionId, VariableId};
use crate::{PropName, Symbol};

use super::Type;

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

impl AssignOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::AddAssign => "+=",
            Self::AndAssign => "&&=",
            Self::BitAndAssign => "&=",
            Self::BitOrAssign => "|=",
            Self::BitXorAssign => "^=",
            Self::DivAssign => "/=",
            Self::ExpAssign => "**=",
            Self::LShiftAssign => "<<=",
            Self::ModAssign => "%=",
            Self::MulAssign => "*=",
            Self::NullishAssign => "??=",
            Self::RShiftAssign => ">>=",
            Self::SubAssign => "-=",
            Self::ZeroFillRShiftAssign => ">>>=",
            Self::OrAssign => "||=",
            Self::Assign => "=",
        }
    }
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

impl BinOp {
    pub fn as_str(self) -> &'static str {
        match self {
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
            Self::RShift => ">>",
            Self::Sub => "-",
            Self::URShift => ">>>",
            Self::Or => "||",
        }
    }
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

pub enum Callee {
    Function(FunctionId),
    Member(Expr, PropNameOrExpr),
    Expr(Expr),
    Super(ClassId),
}

impl Callee {
    pub fn is_member(&self) -> bool {
        match self {
            Self::Member(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    LogicalNot,
    BitNot,
    Typeof,
    Void,
    Minus,
    Plus,
}

impl UnaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::BitNot => "~",
            Self::LogicalNot => "!",
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Typeof => "typeof",
            Self::Void => "void",
        }
    }
}

pub enum PropNameOrExpr {
    PropName(PropName),
    Expr(Box<Expr>, Type),
}

pub enum Expr {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    /// loads an i128
    Bigint(i128),
    /// loads a string
    String(String),
    /// loads a symbol
    Symbol(Symbol),
    Regex(),
    /// function is a static and is initialised
    Function(FunctionId),
    /// a closure captures variables
    Closure(FunctionId),
    /// read the this binding
    This,
    /// constructs an array
    Array {
        values: Vec<Expr>,
    },
    /// constructs a tuple
    Tuple {
        values: Vec<Expr>,
    },
    /// constructs a class
    New {
        class: ClassId,
        args: Vec<Expr>,
    },
    /// calls a function
    Call {
        callee: Box<Callee>,
        args: Vec<Expr>,
        optional: bool,
    },
    /// returns a reference
    Member {
        object: Box<Expr>,
        key: PropNameOrExpr,
        optional: bool,
    },
    /// returns the value with member type
    MemberAssign {
        op: AssignOp,
        object: Box<Expr>,
        key: PropNameOrExpr,
        value: Box<Expr>,
    },
    /// increments or decrements the property
    MemberUpdate {
        op: UpdateOp,
        object: Box<Expr>,
        key: PropNameOrExpr,
    },
    /// returns the value with variable type
    VarAssign {
        op: AssignOp,
        variable: VariableId,
        value: Box<Expr>,
    },
    /// returns the loaded value
    VarLoad {
        span: Span,
        variable: VariableId,
    },
    /// returns the value with
    VarUpdate {
        op: UpdateOp,
        variable: VariableId,
    },
    /// binary operations
    Bin {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// unary operations
    Unary {
        op: UnaryOp,
        value: Box<Expr>,
    },
    /// selects right if test is nullish else left
    Ternary {
        test: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// performs expression and returns last value
    Seq(Box<Expr>, Box<Expr>),
    /// async wait
    Await(Box<Expr>),
    /// yields from generator
    Yield(Box<Expr>),

    /// cast a value to another type.
    /// type of value must be compatable with Type
    Cast(Box<Expr>, Type),
    /// assertion that value is not null.
    /// this may panic at runtime if value is null or undefined
    AssertNonNull(Box<Expr>),
}
