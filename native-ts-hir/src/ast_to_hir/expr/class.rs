use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    pub fn translate_class_expr(&mut self, expr: &swc::ClassExpr) -> Result<(Expr, Type)> {
        // class can only be declared as a type
        return Err(Error::syntax_error(
            expr.span(),
            "class expression not allowed",
        ));
    }
}
