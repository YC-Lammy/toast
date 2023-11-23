use native_js_common::rc::Rc;

use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    untyped_hir::{
        self as uhir, ClassType, EnumType, FunctionType, InterfaceMethod, InterfaceType, Type,
        UnknownId,
    },
    VarId, VarKind,
};

use super::{Result, Translater};

impl Translater {
    pub fn translate_arrow(&mut self, arrow: &swc::ArrowExpr) -> Result<uhir::Expr> {
        todo!()
    }

    pub fn translate_function(&mut self, func: &swc::Function) -> Result<uhir::Function> {
        let mut func_ty = FunctionType {
            visit_fingerprint: 0,
            is_definite: true,
            this_ty: Type::Any,
            generics: Vec::new(),
            params: Vec::new(),
            return_ty: Type::Undefined,
        };

        self.context.new_function();

        if let Some(ty_params) = &func.type_params {
            func_ty.generics = self.translate_generic_params(&ty_params)?;
        }

        for param in &func.params {
            if let Some(id) = param.pat.as_ident() {
                if let Some(ann) = &id.type_ann {
                    let ty = self.translate_ty(&ann.type_ann)?;

                    if let Some(varid) = self.context.add_var(&id.sym, ty.clone()) {
                        func_ty.params.push(ty);
                        self.context.function().params.push(varid);
                    } else {
                        return Err(Error::syntax_error(id.span, "duplicated param name"));
                    }
                } else {
                    return Err(Error::syntax_error(id.span, "missing type annotation"));
                }
            } else {
                return Err(Error::syntax_error(
                    param.span,
                    "destructive function params not allowed.",
                ));
            }
        }

        func_ty.return_ty = if let Some(rty) = &func.return_type {
            self.translate_ty(&rty.type_ann)?
        } else {
            Type::Undefined
        };

        if func.is_async {
            func_ty.return_ty = Type::Promise(Box::new(func_ty.return_ty));
        }

        if func.is_generator {
            func_ty.return_ty = Type::Iterator(Box::new(func_ty.return_ty));
        }

        if let Some(body) = &func.body {
            self.translate_stmts(&body.stmts)?;
        }

        let mut function = self.context.end_function();

        function.is_definite = true;
        function.is_async = func.is_async;
        function.is_generator = func.is_generator;
        function.ty = Rc::new(func_ty);

        return Ok(function);
    }
}
