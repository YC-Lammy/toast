use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    /// translate await expression
    pub fn translate_await_expr(
        &mut self,
        expr: &swc::AwaitExpr,
        _expected: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        // translate the promise
        let (e, mut ty) = self.translate_expr(&expr.arg, None)?;

        // get the result type of promise
        if let Type::Promise(p) = ty {
            ty = *p;
        };

        // get the result type of promise in union
        if let Type::Union(u) = ty {
            // create new union
            let mut v = Vec::with_capacity(u.len());

            for t in u.iter() {
                // type is promise
                if let Type::Promise(p) = t {
                    // push promise result type
                    v.push(p.as_ref().clone());
                } else {
                    // push the type
                    v.push(t.clone())
                }
            }
            // write union
            ty = Type::Union(v.into())
        }

        Ok((Expr::Await(Box::new(e)), ty))
    }
}
