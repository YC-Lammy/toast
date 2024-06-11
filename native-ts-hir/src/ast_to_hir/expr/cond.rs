use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    pub fn translate_cond_expr(&mut self, cond: &swc::CondExpr) -> Result<(Expr, Type)> {
        let (mut test, test_ty) = self.translate_expr(&cond.test, None)?;
        let (cons, cons_ty) = self.translate_expr(&cond.cons, None)?;
        let (alt, alt_ty) = self.translate_expr(&cond.alt, None)?;

        if test_ty != Type::Bool {
            // cast it to bool
            self.cast(&mut test, &test_ty, &Type::Bool);
        }

        return Ok((
            Expr::Ternary {
                test: Box::new(test),
                left: Box::new(cons),
                right: Box::new(alt),
            },
            cons_ty.union(alt_ty),
        ));
    }
}
