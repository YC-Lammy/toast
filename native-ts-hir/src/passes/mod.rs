use std::collections::HashMap;

use native_js_common::error::Error;
use swc_common::Span;

use crate::untyped_hir::{
    visit::{BreakOrContinue, Visitor},
    GenericId, Type,
};

pub mod alias_resolver;
pub mod generic_class_resolver;
pub mod generic_function_resolver;
pub mod generic_interface_resolver;
pub mod type_check;
pub mod type_normaliser;
pub mod unknown;

struct GenericReplacer {
    pub generics: HashMap<GenericId, Type>,
}

impl Visitor for GenericReplacer {
    type Error = Error<Span>;
    const FINGER_PRINT: usize = 5;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        // replace the generic
        if let Type::Generic(id) = ty {
            // generic is resolved
            if let Some(t) = self.generics.get(id) {
                // replace
                *ty = t.clone();
            }
        }
        return Ok(BreakOrContinue::Continue);
    }
}
