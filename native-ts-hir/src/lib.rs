pub mod typed_hir;
pub mod untyped_hir;

mod context;
pub mod type_checker;
pub mod type_resolver;

mod ast_transform;

use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl VarId {
    pub fn new() -> Self {
        static IDS: AtomicUsize = AtomicUsize::new(0);
        return Self(IDS.fetch_add(1, Ordering::SeqCst));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    Var,
    Let,
    Const,
    Using,
    AwaitUsing,
}

impl From<swc_ecmascript::ast::VarDeclKind> for VarKind {
    fn from(value: swc_ecmascript::ast::VarDeclKind) -> Self {
        match value {
            swc_ecmascript::ast::VarDeclKind::Const => Self::Const,
            swc_ecmascript::ast::VarDeclKind::Let => Self::Let,
            swc_ecmascript::ast::VarDeclKind::Var => Self::Var,
        }
    }
}

/// property name supported
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PropName {
    /// obj.prop
    Ident(String),
    /// obj.#prop
    Private(String),
    /// obj["prop"]
    String(String),
    /// obj[0]
    Int(i32),
    /// obj[Symbol.iterator]
    Symbol(Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Iterator,
    AsyncIterator,
    Dispose,
    AsyncDispose,
    HasInstance,
    IsConcatSpreadable,
    Match,
    MatchAll,
    Replace,
    Search,
    Species,
    Split,
    ToPrimitive,
    ToStringTag,
    Unscopables,
}
