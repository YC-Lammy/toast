use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast::FunctionParam;
use crate::common::VariableId;
use crate::error::Error;
use crate::{
    ast::{Expr, FuncType, Stmt, Type},
    common::FunctionId,
};

use super::Transformer;

type Result<T> = std::result::Result<T, Error>;

impl Transformer {
    pub(super) fn translate_arrow(
        &mut self,
        func: &swc::ArrowExpr,
        expected: Option<&FuncType>,
    ) -> Result<(Expr, Type)> {
        let id = FunctionId::new();

        let mut func_ty = FuncType {
            this_ty: self.this_ty.clone(),
            params: Vec::new(),
            var_arg: false,
            return_ty: Type::Undefined,
        };

        // function params
        for p in func.params.iter() {
            if let Some(ident) = p.as_ident() {
                if ident.type_ann.is_none() {
                    return Err(Error::syntax_error(ident.span, "missing type annotation"));
                }
                let mut ty = self.translate_type(&ident.type_ann.as_ref().unwrap().type_ann)?;

                if ident.optional {
                    ty = ty.union(Type::Undefined);
                }
                func_ty.params.push(ty);
            } else {
                return Err(Error::syntax_error(
                    p.span(),
                    "destructive param is not allowed",
                ));
            }
        }

        let mut return_ty = match &func.return_type {
            Some(ann) => Some(self.translate_type(&ann.type_ann)?),
            None => None,
        };

        let is_hoisted =
            self.context
                .open_function(func.span, id, func.is_async, func.is_generator);

        assert!(!is_hoisted, "arrow function should not be hoisted");

        match func.body.as_ref() {
            swc::BlockStmtOrExpr::Expr(e) => {
                let (mut expr, ty) = self.translate_expr(e, return_ty.as_ref())?;

                let mut need_cast = false;

                if return_ty.is_none() {
                    if let Some(expected) = expected {
                        self.type_check(e.span(), &ty, &expected.return_ty)?;
                        return_ty = Some(expected.return_ty.clone());

                        need_cast = expected.return_ty != ty;
                    } else {
                        return_ty = Some(self.generalise_type(&mut expr, &ty).unwrap_or(ty));
                    }
                }

                if need_cast {
                    self.context
                        .func()
                        .stmts
                        .push(Stmt::Return(Box::new(Expr::Cast(
                            Box::new(expr),
                            expected.unwrap().return_ty.clone(),
                        ))))
                } else {
                    // simply return
                    self.context.func().stmts.push(Stmt::Return(Box::new(expr)));
                }
            }
            swc::BlockStmtOrExpr::BlockStmt(b) => {
                if return_ty.is_none() {
                    return_ty = Some(Type::Undefined);

                    if let Some(expected) = expected {
                        return_ty = Some(expected.return_ty.clone());
                    }
                }
                self.return_ty = return_ty.as_ref().cloned().unwrap();

                self.translate_block_stmt(b, None)?;
            }
        }

        func_ty.return_ty = return_ty.unwrap();

        let func_id = self.context.end_function();
        debug_assert!(func_id == id);

        return Ok((Expr::Closure(id), Type::Function(func_ty.into())));
    }

    pub(super) fn translate_function(
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
