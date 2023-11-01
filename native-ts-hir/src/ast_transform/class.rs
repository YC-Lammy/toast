use std::rc::Rc;

use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    untyped_hir::{
        self as uhir, ClassType, EnumType, InterfaceMethod, InterfaceType, Type, UnknownId,
    },
    VarId, VarKind,
};

use super::{Result, Translater};


impl Translater{
    pub fn translate_class(&mut self, class: &swc::Class) -> Result<ClassType>{

    }
}