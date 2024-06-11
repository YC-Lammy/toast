use native_ts_parser::swc_core::common::Span;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, Type};

use super::Result;

impl Transformer {
    pub fn translate_instantiation_expr(
        &mut self,
        expr: &swc::TsInstantiation,
    ) -> Result<(Expr, Type)> {
        match expr.expr.as_ref() {
            swc::Expr::Ident(ident) => {
                self.translate_instantiation_ident_expr(expr.span, ident, &expr.type_args)
            }
            swc::Expr::Member(m) => {
                self.translate_instantiation_member_expr(expr.span, m, &expr.type_args)
            }
            _ => return Err(Error::syntax_error(expr.span, "expecting 0 type arguments")),
        }
    }

    fn translate_instantiation_ident_expr(
        &mut self,
        span: Span,
        ident: &swc::Ident,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<(Expr, Type)> {
        todo!()
    }

    fn translate_instantiation_member_expr(
        &mut self,
        span: Span,
        member: &swc::MemberExpr,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<(Expr, Type)> {
        todo!()
    }
}
