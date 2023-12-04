pub mod ast;
mod checks;
mod common;
mod symbol_table;
pub mod transform;

//pub mod passes;

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

impl core::fmt::Display for PropName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(id) => f.write_str(&id),
            Self::String(s) => {
                f.write_str("\"")?;
                f.write_str(s)?;
                f.write_str("\"")
            }
            Self::Int(i) => {
                let mut buf = native_js_common::itoa::Buffer::new();
                f.write_str(buf.format(*i))
            }
            Self::Private(p) => {
                f.write_str("#")?;
                f.write_str(p)
            }
            Self::Symbol(s) => s.fmt(f),
        }
    }
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

impl core::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Symbol.")?;

        f.write_str(match self {
            Self::AsyncDispose => "asyncDispose",
            Self::AsyncIterator => "asyncIterator",
            Self::Dispose => "dispose",
            Self::HasInstance => "hasInstance",
            Self::IsConcatSpreadable => "isConcatSpreadable",
            Self::Iterator => "iterator",
            Self::Match => "match",
            Self::MatchAll => "matchAll",
            Self::Replace => "replace",
            Self::Search => "search",
            Self::Species => "species",
            Self::Split => "split",
            Self::ToPrimitive => "toPrimitive",
            Self::ToStringTag => "toStringTag",
            Self::Unscopables => "unscopables",
        })
    }
}
