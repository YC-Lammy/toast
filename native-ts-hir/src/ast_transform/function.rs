use std::rc::Rc;

use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    untyped_hir::{
        self as uhir, ClassType, EnumType, InterfaceMethod, InterfaceType, Type, UnknownId, FunctionType,
    },
    VarId, VarKind,
};

use super::{Result, Translater};


impl Translater{
    pub fn translate_function(&mut self, func: &swc::Function) -> Result<uhir::Function>{
        let mut func_ty = FunctionType{
            is_definite: true,
            this_ty: Type::Any,
            generics: Vec::new(),
            params: Vec::new(),
            return_ty: Type::Undefined
        };

        self.context.new_function();

        todo!();

        let mut func = self.context.end_function();

        func.ty = Rc::new(func_ty);

        return Ok(func)
    }
}