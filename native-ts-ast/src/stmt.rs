pub use pegy::Parse;
use pegy::Span;

use crate::utils::*;

pub enum Stmt{

}

#[derive(Debug, Default, Parse)]
#[grammar($span:("continue" ";") | $span:("continue" !LineTerminator))]
pub struct ContinueStmt{
    pub span: Span,
    pub label: Option<LabelIdentifier>,
}

#[derive(Debug, Default)]
pub struct LabelIdentifier{

}