use native_ts_parser::swc_core::ecma::ast as swc;

use num_traits::ToPrimitive;

use super::Result;
use crate::ast_to_hir::Transformer;
use crate::hir::{Expr, Type};

impl Transformer {
    pub fn translate_lit_expr(
        &mut self,
        lit: &swc::Lit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match lit {
            swc::Lit::BigInt(b) => {
                let i = b.value.to_i128().expect("i128 overflow");
                Ok((Expr::Bigint(i), Type::LiteralBigint(i)))
            }
            swc::Lit::Bool(b) => Ok((Expr::Bool(b.value), Type::LiteralBool(b.value))),
            swc::Lit::JSXText(_) => unimplemented!(),
            swc::Lit::Null(_) => Ok((Expr::Null, Type::Null)),
            swc::Lit::Num(n) => {
                if expected_ty == Some(&Type::Number) {
                    return Ok((Expr::Number(n.value), Type::Number));
                }
                if n.value.is_finite() && n.value as i32 as f64 == n.value {
                    return Ok((Expr::Int(n.value as i32), Type::LiteralInt(n.value as i32)));
                }

                return Ok((Expr::Number(n.value), Type::LiteralNumber(n.value.into())));
            }
            swc::Lit::Str(s) => Ok((
                Expr::String(s.value.to_string()),
                Type::LiteralString(s.value.as_str().into()),
            )),
            // todo: regex
            swc::Lit::Regex(_r) => Ok((Expr::Regex(), Type::Regex)),
        }
    }
}
