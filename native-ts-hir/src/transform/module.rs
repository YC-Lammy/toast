use crate::common::ModuleId;
use crate::PropName;

use super::{context::Binding, Transformer};

impl Transformer {
    pub fn find_binding_from_module(&mut self, id: ModuleId, name: &PropName) -> Option<Binding> {
        todo!()
    }
}
