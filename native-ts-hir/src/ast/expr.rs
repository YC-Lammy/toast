use native_ts_parser::swc_core::common::Span;

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

impl From<native_ts_parser::swc_core::ecma::ast::AssignOp> for AssignOp {
    fn from(value: native_ts_parser::swc_core::ecma::ast::AssignOp) -> Self {
        match value {
            native_ts_parser::swc_core::ecma::ast::AssignOp::AddAssign => Self::AddAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::AndAssign => Self::AndAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::BitAndAssign => Self::BitAndAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::BitOrAssign => Self::BitOrAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::BitXorAssign => Self::BitXorAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::DivAssign => Self::DivAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::ExpAssign => Self::ExpAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::LShiftAssign => Self::LShiftAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::ModAssign => Self::ModAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::MulAssign => Self::MulAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::NullishAssign => Self::NullishAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::RShiftAssign => Self::RShiftAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::SubAssign => Self::SubAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::ZeroFillRShiftAssign => Self::ZeroFillRShiftAssign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::Assign => Self::Assign,
            native_ts_parser::swc_core::ecma::ast::AssignOp::OrAssign => Self::OrAssign,
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

impl From<native_ts_parser::swc_core::ecma::ast::BinaryOp> for BinOp {
    fn from(value: native_ts_parser::swc_core::ecma::ast::BinaryOp) -> Self {
        match value {
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Add => Self::Add,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::BitAnd => Self::BitAnd,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::BitOr => Self::BitOr,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::BitXor => Self::BitXor,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Div => Self::Div,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::EqEq => Self::EqEq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::EqEqEq => Self::EqEqEq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Exp => Self::Exp,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Gt => Self::Gt,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::GtEq => Self::Gteq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::In => Self::In,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::InstanceOf => unreachable!(),
            native_ts_parser::swc_core::ecma::ast::BinaryOp::LShift => Self::LShift,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::LogicalAnd => Self::And,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::LogicalOr => Self::Or,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Lt => Self::Lt,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::LtEq => Self::Lteq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Mod => Self::Mod,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Mul => Self::Mul,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::NotEq => Self::NotEq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::NotEqEq => Self::NotEqEq,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::NullishCoalescing => Self::Nullish,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::RShift => Self::RShift,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::Sub => Self::Sub,
            native_ts_parser::swc_core::ecma::ast::BinaryOp::ZeroFillRShift => Self::URShift,
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

#[derive(Debug, Clone)]
pub enum Callee {
    Function(FunctionId),
    Member { object: Expr, prop: PropNameOrExpr },
    Expr(Expr),
    Super(ClassId),
}

impl Callee {
    pub fn is_member(&self) -> bool {
        match self {
            Self::Member { .. } => true,
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

#[derive(Debug, Clone)]
pub enum PropNameOrExpr {
    PropName(PropName),
    Expr(Box<Expr>, Type),
}

#[derive(Debug, Clone)]
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
    /// function is static and is initialised
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
