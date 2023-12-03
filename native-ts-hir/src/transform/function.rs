use native_js_common::error::Error;
use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    ast::{Expr, FuncType, Stmt, Type},
    common::FunctionId,
};

use super::Transformer;

type Result<T> = std::result::Result<T, Error<Span>>;

impl Transformer {
    pub fn translate_arrow(
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

        for p in &func.params {
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

        self.context.new_function(id);

        match func.body.as_ref() {
            swc::BlockStmtOrExpr::Expr(e) => {
                let (expr, ty) = self.translate_expr(e, return_ty.as_ref())?;

                let mut need_cast = false;

                if return_ty.is_none() {
                    if let Some(expected) = expected {
                        self.type_check(e.span(), &ty, &expected.return_ty)?;
                        return_ty = Some(expected.return_ty.clone());

                        need_cast = expected.return_ty != ty;
                    } else {
                        return_ty = Some(ty);
                    }
                }

                if need_cast {
                    self.context.func().stmts.push(Stmt::Return(Expr::Cast(
                        Box::new(expr),
                        expected.unwrap().return_ty.clone(),
                    )))
                } else {
                    // simply return
                    self.context.func().stmts.push(Stmt::Return(expr));
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

        self.context.end_function();

        func_ty.return_ty = return_ty.unwrap();

        return Ok((Expr::Closure(id), Type::Function(Box::new(func_ty))));
    }

    pub fn translate_function(&mut self, id: FunctionId, func: &swc::Function) -> Result<()> {
        todo!()
    }
}
