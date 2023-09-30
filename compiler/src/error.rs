use std::borrow::Cow;

use swc_common::{SourceMap, Span};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: Cow<'static, str>,
    pub note: Cow<'static, str>,
}

impl Error {
    pub fn new<A, B>(span: Span, msg: A, note: B) -> Self
    where
        A: Into<Cow<'static, str>>,
        B: Into<Cow<'static, str>>,
    {
        Self {
            span,
            msg: msg.into(),
            note: note.into(),
        }
    }

    pub fn display(&self, filemap: &SourceMap) -> String {
        let location = if self.span.is_dummy() {
            "unknown".to_string()
        } else {
            filemap.span_to_string(self.span)
        };

        format!(
            "error: {}\n  --> {}\nnote: {}",
            self.msg, location, self.note
        )
    }
}
