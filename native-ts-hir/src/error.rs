use native_ts_parser::swc_core::common::Span;

use crate::ast::Type;

#[derive(Debug, Clone)]
pub enum Error {
    SyntaxError {
        span: Span,
        msg: String,
    },
    TypeError {
        span: Span,
        expected: Type,
        actual: Type,
        msg: String,
    },
    CompilerError {
        span: Span,
        msg: String,
    },
}

impl Error {
    pub fn syntax_error<S: Into<String>>(span: Span, msg: S) -> Self {
        Self::SyntaxError {
            span: span,
            msg: msg.into(),
        }
    }

    pub fn type_error<S: Into<String>>(span: Span, expected: Type, actual: Type, msg: S) -> Self {
        Self::TypeError {
            span,
            expected,
            actual,
            msg: msg.into(),
        }
    }
    pub fn compiler_error<S: Into<String>>(span: Span, msg: S) -> Self {
        Self::SyntaxError {
            span: span,
            msg: msg.into(),
        }
    }
}
