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

    pub fn format_error(&self, map: &native_ts_parser::SourceMap) -> String {
        let mut buf = String::new();
        let mut itoa_buf = itoa::Buffer::new();

        buf.push_str("error: ");

        let span = match self {
            Self::SyntaxError { span, msg } => {
                buf.push_str("SyntaxError: ");
                buf.push_str(&msg);

                *span
            }
            Self::TypeError {
                span,
                expected,
                actual,
                msg,
            } => {
                buf.push_str("TypeError: ");
                buf.push_str(&msg);
                buf.push_str("\n    type ");
                buf.push_str(" is not assignable to type ");

                *span
            }
            _ => todo!(),
        };

        buf.push_str("\n  -->");

        let loc = map.lookup_char_pos(span.lo);
        let end_loc = map.lookup_char_pos(span.hi);

        buf.push_str(&loc.file.name.to_string());
        buf.push(':');
        buf.push_str(itoa_buf.format(loc.line));
        buf.push(':');
        buf.push_str(itoa_buf.format(loc.col.0));
        buf.push('\n');

        let start = loc.line;
        let end = end_loc.line;

        println!("{}, {}", start, end);

        for line in start..=end {
            if let Some(src) = loc.file.get_line(line - 1) {
                buf.push_str("        | \n");
                let line_s = itoa_buf.format(line);
                buf.push_str(line_s);
                for _ in 0..(8 - line_s.len()) {
                    buf.push(' ');
                }
                buf.push_str("| ");

                buf.push_str(&src);

                if Some(&('\n' as u8)) != src.as_bytes().last() {
                    buf.push('\n');
                }

                buf.push_str("        | ");

                for c in 0..src.len() {
                    if line != end && c >= loc.col.0 {
                        buf.push('^');
                    } else if line == end && start != end && c <= end_loc.col.0 {
                        buf.push('^');
                    } else if line == end && start == end && c <= end_loc.col.0 && c >= loc.col.0 {
                        buf.push('^');
                    } else {
                        buf.push(' ');
                    }
                }

                buf.push('\n')
            } else {
                break;
            }
        }

        return buf;
    }
}
