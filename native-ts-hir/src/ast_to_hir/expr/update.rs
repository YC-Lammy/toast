use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    /// translate update expression
    pub fn translate_update_expr(&mut self, u: &swc::UpdateExpr) -> Result<(Expr, Type)> {
        // translate the target
        let (expr, ty) = self.translate_expr(&u.arg, None)?;

        let op = match u.op {
            swc::UpdateOp::MinusMinus => {
                if u.prefix {
                    crate::hir::UpdateOp::PrefixSub
                } else {
                    crate::hir::UpdateOp::SuffixSub
                }
            }
            swc::UpdateOp::PlusPlus => {
                if u.prefix {
                    crate::hir::UpdateOp::PrefixAdd
                } else {
                    crate::hir::UpdateOp::SuffixAdd
                }
            }
        };

        match ty {
            Type::Int | Type::Number | Type::Bigint => {}
            Type::LiteralInt(_) | Type::LiteralNumber(_) | Type::LiteralBigint(_) => {
                return Err(Error::syntax_error(
                    u.span,
                    "literal types cannot be updated",
                ))
            }
            _ => {
                return Err(Error::syntax_error(
                    u.span,
                    "operand must have type 'number' or 'bigint'",
                ))
            }
        }

        // target expression must either be member or variable
        match expr {
            // target is member
            Expr::Member {
                object,
                key,
                optional,
                ..
            } => {
                // member cannot be opt chain
                if optional {
                    return Err(Error::syntax_error(
                        u.span,
                        "invalid left-hand side assignment",
                    ));
                }

                // return update expression
                return Ok((
                    Expr::MemberUpdate {
                        span: u.span,
                        op: op,
                        object: object,
                        key: key,
                    },
                    ty,
                ));
            }
            // target is a variable
            Expr::VarLoad { variable, span: _ } => {
                // return update expression
                return Ok((
                    Expr::VarUpdate {
                        span: u.span,
                        op: op,
                        variable: variable,
                    },
                    ty,
                ));
            }
            _ => {
                return Err(Error::syntax_error(
                    u.span,
                    "invalid left-hand side assignment",
                ))
            }
        };
    }
}
