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

impl From<swc_ecmascript::ast::AssignOp> for AssignOp{
    fn from(value: swc_ecmascript::ast::AssignOp) -> Self {
        match value{
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
            swc_ecmascript::ast::AssignOp::OrAssign => Self::OrAssign
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

pub enum Callee{
    Function(FunctionId),
    Member(Expr, PropName),
    Expr(Expr),
    Super(ClassId),
}

impl Callee{
    pub fn is_member(&self) -> bool{
        match self{
            Self::Member(_, _) => true,
            _ => false
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

pub enum Expr {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    String(String),
    Symbol(Symbol),
    Regex {},
    Function(FunctionId),
    This,
    Array {
        values: Vec<Expr>,
    },
    New {
        class: ClassId,
        args: Vec<Expr>,
    },
    Call {
        callee: Box<Callee>,
        args: Vec<Expr>,
        optchain: bool,
    },
    /// returns a reference
    Member {
        object: Box<Expr>,
        key: PropName,
    },
    /// returns a reference | undefined
    OptMember {
        object: Box<Expr>,
        key: PropName,
    },
    /// returns the value with member type
    MemberAssign {
        op: AssignOp,
        object: Box<Expr>,
        key: PropName,
        value: Box<Expr>,
    },
    MemberUpdate {
        op: UpdateOp,
        object: Box<Expr>,
        key: PropName,
    },
    /// returns the value with variable type
    VarAssign {
        op: AssignOp,
        variable: VariableId,
        value: Box<Expr>,
    },
    /// returns the loaded value
    VarLoad {
        variable: VariableId,
    },
    /// returns the value with
    VarUpdate {
        op: UpdateOp,
        variable: VariableId,
    },
    Bin {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        value: Box<Expr>,
    },
    Ternary {
        test: Box<Expr>,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Seq(Box<Expr>, Box<Expr>),

    Await(Box<Expr>),
    Yield(Box<Expr>),
    Cast(Box<Expr>, Type),
}
