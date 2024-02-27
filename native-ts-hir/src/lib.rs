/// HIR definitions
pub mod ast;
/// post transformation checks
mod checks;
/// utils
mod common;
/// symbol table data structure
mod symbol_table;
/// transforms AST to HIR
pub mod transform;

use std::sync::atomic::{AtomicUsize, Ordering};

/// a unique identifier for variables
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl VarId {
    /// creates a new unique identifier
    pub fn new() -> Self {
        // static counter
        static IDS: AtomicUsize = AtomicUsize::new(0);
        // fetch and increment counter
        return Self(IDS.fetch_add(1, Ordering::SeqCst));
    }
}

/// variable kind, not included in `ast` because it is not part of ast.
///
/// this is only used during the translation process for syntax checks
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    /// `var` declare, can be redeclared but must be of same type
    Var,
    /// `let` declare, cannot be redeclared
    Let,
    /// `const` declare, readonly
    Const,
    /// `using` declare, readonly and owned by the scope.
    /// destructor called when it goes out of scope
    Using,
    /// `await using` declare. Same as `using` but with async destructor
    AwaitUsing,
}

/// implemented for convenience
impl From<native_ts_parser::swc_core::ecma::ast::VarDeclKind> for VarKind {
    fn from(value: native_ts_parser::swc_core::ecma::ast::VarDeclKind) -> Self {
        match value {
            native_ts_parser::swc_core::ecma::ast::VarDeclKind::Const => Self::Const,
            native_ts_parser::swc_core::ecma::ast::VarDeclKind::Let => Self::Let,
            native_ts_parser::swc_core::ecma::ast::VarDeclKind::Var => Self::Var,
        }
    }
}

/// property name for attributes and methods
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PropName {
    /// e.g. obj.prop
    Ident(String),
    /// e.g. obj.#prop
    Private(String),
    /// e.g. obj["prop"]
    String(String),
    /// e.g. obj[0]
    Int(i32),
    /// e.g. obj[Symbol.iterator]
    Symbol(Symbol),
}

/// format propname
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

/// Typescript builtin symbols
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
