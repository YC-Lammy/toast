use native_js_common::error::Error;
use swc_common::Span;

use crate::untyped_hir::{visit::{Visitor, BreakOrContinue}, Type};




pub struct TypeNormaliser{

}

impl Visitor for TypeNormaliser{
    const FINGER_PRINT: usize = 10;
    type Error = Error<Span>;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        match ty{
            _ => {}
        }
        return Ok(BreakOrContinue::Continue)
    }
}