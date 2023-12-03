use swc_ecmascript::ast as swc;

use crate::PropName;
use crate::{ast::ModuleExport, common::ModuleId};

use super::{context::Binding, Transformer};

impl Transformer {
    pub fn find_binding_from_module(&mut self, id: ModuleId, name: &PropName) -> Option<Binding> {
        let module_export = self.module_export(id, name)?;

        let bind = match module_export {
            ModuleExport::Class(id) => Binding::Class(id),
            ModuleExport::Function(id) => Binding::Function(id),
            ModuleExport::Interface(id) => Binding::Interface(id),
            ModuleExport::NameSpace(n) => Binding::NameSpace(n),
            ModuleExport::Enum(e) => Binding::Enum(e),
            ModuleExport::Alias(a) => Binding::TypeAlias(a),
            ModuleExport::Undefined => return None,
            ModuleExport::Var(id, ty) => Binding::Var {
                writable: false,
                redeclarable: false,
                id: id,
                ty: ty,
            },
        };
        return Some(bind);
    }

    pub fn find_module(&mut self, name: &str) -> ModuleId {
        todo!()
    }

    pub fn module_default_export(&self, module: ModuleId) -> ModuleExport {
        todo!()
    }

    pub fn module_export(&self, module: ModuleId, name: &PropName) -> Option<ModuleExport> {
        todo!()
    }

    pub fn translate_module_export_name(&mut self, name: &swc::ModuleExportName) -> PropName {
        match name {
            swc::ModuleExportName::Ident(id) => PropName::Ident(id.sym.to_string()),
            swc::ModuleExportName::Str(s) => PropName::String(s.value.to_string()),
        }
    }
}
