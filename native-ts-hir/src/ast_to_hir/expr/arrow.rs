use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::common::FunctionId;
use crate::error::Error;
use crate::hir::{Expr, FuncType, Stmt, Type};

use super::Result;

impl Transformer {
    /// translates the arrow expression
    pub fn translate_arrow_expr(
        &mut self,
        arrow: &swc::ArrowExpr,
        expected: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        // filter out non function types
        let expected = match expected {
            Some(Type::Function(f)) => Some(f.as_ref()),
            None => None,
            _ => None,
        };
        // translate the arrow expression
        self.translate_arrow(arrow, expected)
    }

    /// translate the arrow expression
    pub(super) fn translate_arrow(
        &mut self,
        func: &swc::ArrowExpr,
        expected: Option<&FuncType>,
    ) -> Result<(Expr, Type)> {
        let id = FunctionId::new();

        // construct a function type
        let mut func_ty = FuncType {
            // arrow captures 'this'
            this_ty: self.this_ty.clone(),
            params: Vec::new(),
            var_arg: false,
            return_ty: Type::Undefined,
        };

        // function params
        for (i, p) in func.params.iter().enumerate() {
            // only accept ident params
            if let Some(ident) = p.as_ident() {
                let mut ty =
                // check if param is annotated
                if ident.type_ann.is_none() {
                    // get the expected type if not annotated
                    let expected_param_ty = match expected{
                        Some(f) => f.params.get(i),
                        None => None
                    };
                    match expected_param_ty{
                        Some(ty) => ty.clone(),
                        None => return Err(Error::syntax_error(ident.span, "missing type annotation"))
                    }

                } else{
                    // translate the param type if annotated
                    self.translate_type(&ident.type_ann.as_ref().unwrap().type_ann)?
                };

                // union with undefined if optional
                if ident.optional {
                    ty = ty.union(Type::Undefined);
                }
                // push param type to function type
                func_ty.params.push(ty);
            } else {
                // todo: destructive params
                return Err(Error::syntax_error(
                    p.span(),
                    "destructive param is not allowed",
                ));
            }
        }

        // translate the return type
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
                        .push(Stmt::Return(Box::new(Expr::Cast {
                            span: func.span,
                            value: Box::new(expr),
                            ty: expected.unwrap().return_ty.clone(),
                        })))
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
}
