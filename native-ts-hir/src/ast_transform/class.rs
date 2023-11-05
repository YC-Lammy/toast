use native_js_common::rc::Rc;

use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    VarId, VarKind,
};
use crate::untyped_hir::{
    ClassType, EnumType, InterfaceMethod, InterfaceType, Type, UnknownId,
};
use super::{
    Result, Translater
};

impl Translater {
    pub fn translate_class(&mut self, class: &swc::Class) -> Result<ClassType> {
        let mut class_ty = ClassType{
            name: String::new(),
            visit_fingerprint: 0,
            is_definite: true,
            generics: Vec::new(),
            extends: None,
            implements: Vec::new(),
            attributes: Vec::new(),
            methods: Vec::new(),
            static_functions: Vec::new(),
            static_props: Vec::new()
        };

        self.context.new_scope();

        if let Some(params) = &class.type_params{
            class_ty.generics = self.translate_generic_params(params)?;
        };

        for i in &class.implements{
            let mut type_args = Vec::new();

            if let Some(args) = &i.type_args{
                for arg in &args.params{
                    type_args.push(self.translate_ty(arg)?);
                }
            }
            let imp = self.translate_type_expr(&i.expr, type_args)?;

            class_ty.implements.push(imp);
        }

        if let Some(super_ty) = &class.super_class{
            let mut super_type_param = Vec::new();
            if let Some(params) = &class.super_type_params{
                for p in &params.params{
                    super_type_param.push(self.translate_ty(&p)?);
                }
            }

            let super_ty = self.translate_type_expr(&super_ty, super_type_param)?;
            class_ty.extends = Some(super_ty);
        }

        for item in &class.body{
            
        }

        self.context.close_scope();

        return Ok(class_ty);
    }
}
