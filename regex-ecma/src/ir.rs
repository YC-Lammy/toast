

use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum RegexIR {
    /// never used
    Invalid,
    /// add one to cursor
    IncrementCursor,
    /// sub one from cursor
    DecrementCursor,
    /// save the current position to stack
    SaveCursor,
    /// restore the saved position
    RestoreCursor,
    /// match a single byte
    Char(u8),
    /// match a char code
    Char32(u32),
    /// dot does not match line terminator
    Dot,
    /// same as dot but match any character including line terminator
    Any,
    /// line start, last character must be line end or position is zero
    LineStart,
    /// line end
    LindEnd,

    /// save start position to a capture index
    SaveStart(u32),
    /// save end position to a capture index
    SaveEnd(u32),
    /// back reference a capture index, increments the cursor
    BackReference(u32),
    /// back reference a capture in backwards, decrements the cursor
    BackwardBackReference(u32),

    /// a word boundary
    WordBoundary,
    /// not a word boundary
    NotWordBoundary,
    
    /// variable length
    Range(Box<[core::ops::Range<u8>]>),
    /// variable length
    Range32(Box<[std::ops::Range<u32>]>),
}


pub struct IRGenerator{
    dotall: bool,
    multiline: bool,
    unicode: bool,

    capture_count: usize,

    last_atom_start: i64,
    last_capture_count: i64,

    ir: Vec<RegexIR>,
}