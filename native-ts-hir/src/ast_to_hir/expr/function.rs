use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::common::{FunctionId, VariableId};
use crate::error::Error;
use crate::hir::{Expr, FunctionParam, Type};

use super::Result;

impl Transformer {
    /// translate a function expression
    pub fn translate_func_expr(&mut self, expr: &swc::FnExpr) -> Result<(Expr, Type)> {
        let id = self.hoist_function(None, &expr.function)?;
        self.translate_function(id, None, &expr.function)?;

        let ty = self
            .context
            .functions
            .get(&id)
            .expect("invalid function")
            .ty();

        Ok((Expr::Closure(id), Type::Function(ty.into())))
    }

    pub fn translate_function(
        &mut self,
        id: FunctionId,
        class_this_ty: Option<Type>,
        func: &swc::Function,
    ) -> Result<()> {
        let is_hoisted =
            self.context
                .open_function(func.span, id, func.is_async, func.is_generator);

        let mut this_ty = Type::Any;

        if let Some(_type_params) = &func.type_params {
            todo!("generic function")
        }

        for (i, p) in func.params.iter().enumerate() {
            if let Some(ident) = p.pat.as_ident() {
                let (id, ty) = if is_hoisted {
                    (
                        self.context.func().params[i].id,
                        self.context.func().params[i].ty.clone(),
                    )
                } else {
                    let id = VariableId::new();
                    let ty = if let Some(ann) = &ident.type_ann {
                        // translate the type annotation
                        let ty = self.translate_type(&ann.type_ann)?;

                        if ident.optional {
                            ty.union(Type::Undefined)
                        } else {
                            ty
                        }
                    } else {
                        return Err(Error::syntax_error(ident.span, "missing type annotation"));
                    };

                    self.context.func().params.push(FunctionParam {
                        id: id,
                        ty: ty.clone(),
                    });

                    (id, ty)
                };

                // declare binding
                self.context.bind_variable(&ident.sym, id, ty, true, true);
            }
        }

        if let Some(ty) = class_this_ty {
            this_ty = ty;
        }

        let mut return_ty = if let Some(ann) = &func.return_type {
            self.translate_type(&ann.type_ann)?
        } else {
            Type::Undefined
        };

        if func.is_async {
            return_ty = Type::Promise(Box::new(return_ty));
        }
        if func.is_generator {
            return_ty = Type::Iterator(return_ty.into());
        }

        let old_this_ty = core::mem::replace(&mut self.this_ty, this_ty);
        let old_return_ty = core::mem::replace(&mut self.return_ty, return_ty);

        // translate body
        if let Some(block) = &func.body {
            self.translate_block_stmt(block, None)?
        } else {
            return Err(Error::syntax_error(func.span, "missing function body"));
        }

        let this_ty = core::mem::replace(&mut self.this_ty, old_this_ty);
        let return_ty = core::mem::replace(&mut self.return_ty, old_return_ty);

        let f = self.context.func();
        f.this_ty = this_ty;
        f.return_ty = return_ty;

        let func_id = self.context.end_function();
        debug_assert!(func_id == id);

        return Ok(());
    }
}
