
use native_js_common::error::Error;
use swc_common::Span;
use swc_ecmascript::ast as swc;

use crate::{
    ast::Type,
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId},
};

use super::Transformer;

type Result<T> = std::result::Result<T, Error<Span>>;

impl Transformer{
    pub fn translate_function(&mut self, id: FunctionId, func: &swc::Function) -> Result<()>{
        todo!()
    }
}