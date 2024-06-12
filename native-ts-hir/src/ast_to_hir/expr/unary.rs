use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, Type};

use super::Result;

impl Transformer {
    /// translate unary operation expression
    pub fn translate_unary_expr(&mut self, u: &swc::UnaryExpr) -> Result<(Expr, Type)> {
        let (mut expr, ty) = self.translate_expr(&u.arg, None)?;
        match u.op {
            swc::UnaryOp::Bang => {
                // type check not needed, all types can cast to bool
                if ty != Type::Bool {
                    // cast type to bool
                    self.cast(&mut expr, &ty, &Type::Bool)
                }

                // return expression
                return Ok((
                    Expr::Unary {
                        span: u.span,
                        op: crate::hir::UnaryOp::LogicalNot,
                        value: Box::new(expr),
                    },
                    Type::Bool,
                ));
            }
            swc::UnaryOp::Delete => {
                // delete is dynamic and should not be allowed
                // todo: unary delete
                return Err(Error::syntax_error(u.span, "'delete' is not allowed"));
            }
            swc::UnaryOp::Minus | swc::UnaryOp::Plus => {
                match ty {
                    Type::Number
                    | Type::LiteralNumber(_)
                    | Type::Int
                    | Type::LiteralInt(_)
                    | Type::Bigint
                    | Type::LiteralBigint(_) => {
                        // return unary expression
                        return Ok((
                            Expr::Unary {
                                span: u.span,
                                op: if u.op == swc::UnaryOp::Minus {
                                    crate::hir::UnaryOp::Minus
                                } else {
                                    crate::hir::UnaryOp::Plus
                                },
                                value: Box::new(expr),
                            },
                            match ty {
                                Type::Int => Type::Int,
                                Type::Bigint => Type::Bigint,
                                _ => Type::Number,
                            },
                        ));
                    }
                    _ => {
                        return Err(Error::syntax_error(
                            u.span,
                            "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                        ))
                    }
                }
            }
            swc::UnaryOp::Tilde => {
                match ty {
                    Type::Number => {}
                    Type::Int => {}
                    Type::LiteralNumber(_) => {}
                    Type::LiteralInt(_) => {}
                    Type::Bigint => {}
                    Type::LiteralBigint(_) => {}
                    Type::Bool => {}
                    Type::LiteralBool(_) => {}
                    _ => {
                        return Err(Error::syntax_error(
                            u.span,
                            "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                        ));
                    }
                }

                // cast value to int
                self.cast(&mut expr, &ty, &Type::Int);

                // return expression
                return Ok((
                    Expr::Unary {
                        span: u.span,
                        op: crate::hir::UnaryOp::BitNot,
                        value: Box::new(expr),
                    },
                    Type::Int,
                ));
            }
            swc::UnaryOp::TypeOf => {
                // runtime reflect
                return Ok((
                    Expr::Unary {
                        span: u.span,
                        op: crate::hir::UnaryOp::Typeof,
                        value: Box::new(expr),
                    },
                    Type::String,
                ));
            }
            swc::UnaryOp::Void => {
                // simply return undefined
                return Ok((
                    Expr::Unary {
                        span: u.span,
                        op: crate::hir::UnaryOp::Void,
                        value: Box::new(expr),
                    },
                    Type::Undefined,
                ));
            }
        }
    }
}
