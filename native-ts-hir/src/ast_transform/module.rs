use std::path::Path;

use native_ts_parser::ModuleId;

use crate::context::Binding;

use super::Translater;

impl Translater {
    pub fn find_module(&self, current_path: &Path, module_name: &str) -> Option<ModuleId> {
        todo!()
    }

    pub fn find_module_export(&mut self, module: ModuleId, export_name: &str) -> Option<Binding> {
        todo!()
    }

    pub fn module_exports(&mut self, module: ModuleId) -> Vec<(String, Binding)> {
        todo!()
    }
}
