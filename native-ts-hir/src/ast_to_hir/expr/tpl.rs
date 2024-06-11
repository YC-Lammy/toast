use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    pub fn translate_tpl_expr(&mut self, expr: &swc::Tpl) -> Result<(Expr, Type)> {
        // todo: template literal
        return Err(Error::compiler_error(
            expr.span,
            "template literal not supported",
        ));
    }
    pub fn translate_tagged_tpl_expr(&mut self, expr: &swc::TaggedTpl) -> Result<(Expr, Type)> {
        // todo: template literal
        return Err(Error::compiler_error(
            expr.span,
            "template literal not supported",
        ));
    }
}
