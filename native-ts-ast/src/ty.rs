use pegy::{Span, Parse};


use crate::utils::IdentifierName;

#[derive(Debug, Parse)]
pub enum Type{
    #[grammar($item0: IdentifierName)]
    Ident(IdentifierName),
}

impl Default for Type{
    
}