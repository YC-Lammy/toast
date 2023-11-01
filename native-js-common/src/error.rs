use alloc::{borrow::Cow, boxed::Box};



#[derive(Debug)]
pub enum Error<SP>{
    SyntaxError{
        span: SP,
        msg: Cow<'static, str>
    },
    CompilerError{
        msg: Cow<'static, str>
    },
    Chained{
        first: Box<Self>,
        second: Box<Self>,
    }
}

impl<SP> Error<SP>{
    pub fn syntax_error<S:Into<Cow<'static, str>>>(sp: SP, msg:S) -> Self{
        Self::SyntaxError { span: sp, msg: msg.into() }
    }

    pub fn compiler_error<S:Into<Cow<'static, str>>>(msg: S) -> Self{
        Self::CompilerError { msg: msg.into() }
    }

    pub fn chain(self, other: Self) -> Self{
        Self::Chained { first: Box::new(self), second: Box::new(other) }
    }
}