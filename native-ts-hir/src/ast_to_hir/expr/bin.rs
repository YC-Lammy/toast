use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, Type};

use super::Result;

impl Transformer {
    /// translate binary expression
    pub fn translate_bin_expr(&mut self, b: &swc::BinExpr) -> Result<(Expr, Type)> {
        if b.op == swc::BinaryOp::In {
            let prop = self.translate_computed_prop_name(&b.left)?;
            let (right, right_ty) = self.translate_expr(&b.right, None)?;

            // translate the left hand side
            match prop {
                PropNameOrExpr::PropName(prop) => {
                    if let Some(_) = self.type_has_property(&right_ty, &prop, false) {
                        return Ok((
                            Expr::Seq {
                                seq: vec![right, Expr::Bool(true)],
                            },
                            Type::Bool,
                        ));
                    } else {
                        return Ok((
                            Expr::Seq {
                                seq: vec![right, Expr::Bool(false)],
                            },
                            Type::Bool,
                        ));
                    }
                }
                PropNameOrExpr::Expr(..) => {
                    // todo: computed prop name
                    return Err(Error::syntax_error(
                        b.span,
                        "computed property name 'in' operation not supported",
                    ));
                }
            }
        }
        let (mut left, mut left_ty) = self.translate_expr(&b.left, None)?;
        let (mut right, mut right_ty) = self.translate_expr(&b.right, None)?;

        // treat literal int as int
        if let Type::LiteralInt(_) = left_ty {
            left_ty = Type::Int;
        }
        if let Type::LiteralInt(_) = right_ty {
            right_ty = Type::Int;
        }
        // treat literal number as number
        if let Type::LiteralNumber(_) = left_ty {
            left_ty = Type::Number;
        }
        if let Type::LiteralNumber(_) = right_ty {
            right_ty = Type::Number;
        }
        // treat literal bigint as bigint
        if let Type::LiteralBigint(_) = left_ty {
            left_ty = Type::Bigint;
        }
        if let Type::LiteralBigint(_) = right_ty {
            right_ty = Type::Bigint;
        }
        // treat literal bool as bool
        if let Type::LiteralBool(_) = left_ty {
            left_ty = Type::Bool;
        }
        if let Type::LiteralBool(_) = right_ty {
            right_ty = Type::Bool;
        }
        // treat literal string as string
        if let Type::LiteralString(_) = left_ty {
            left_ty = Type::String;
        }
        if let Type::LiteralString(_) = right_ty {
            right_ty = Type::String;
        }

        // the result type
        let ty;

        // check op and the result type
        match b.op {
            swc::BinaryOp::Add
            | swc::BinaryOp::Sub
            | swc::BinaryOp::Mul
            | swc::BinaryOp::Mod
            | swc::BinaryOp::Exp => {
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    // both are int
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    // cast left hand side as number
                    left_ty = Type::Number;
                    // cast expression
                    self.cast(&mut left, &Type::Int, &Type::Number);
                    // result is number
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    // cast right hand side as number
                    right_ty = Type::Number;
                    // cast expression
                    self.cast(&mut right, &Type::Int, &Type::Number);
                    // result is number
                    ty = Type::Number;
                // both are bigint
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                // both are string
                } else if b.op == swc::BinaryOp::Add
                    && left_ty == Type::String
                    && right_ty == Type::String
                {
                    ty = Type::String
                } else {
                    // unsupported types
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // right should be equal to left
                debug_assert!(right_ty == left_ty);
                debug_assert!(
                    left_ty == Type::Int
                        || left_ty == Type::Number
                        || left_ty == Type::Bigint
                        || left_ty == Type::String
                );
            }
            swc::BinaryOp::Div => {
                if left_ty == Type::Int {
                    left_ty = Type::Number;
                    self.cast(&mut left, &Type::Int, &Type::Number);
                }
                if right_ty == Type::Int {
                    right_ty = Type::Number;
                    self.cast(&mut right, &Type::Int, &Type::Number);
                }
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                } else {
                    // unsupported types
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // right should be equal to left
                debug_assert!(right_ty == left_ty);
                debug_assert!(
                    left_ty == Type::Int || left_ty == Type::Number || left_ty == Type::Bigint
                );
            }
            swc::BinaryOp::BitAnd
            | swc::BinaryOp::BitOr
            | swc::BinaryOp::BitXor
            | swc::BinaryOp::LShift
            | swc::BinaryOp::RShift
            | swc::BinaryOp::ZeroFillRShift => {
                if left_ty == Type::Number {
                    // cast to int
                    left_ty = Type::Int;
                    self.cast(&mut left, &Type::Number, &Type::Int);
                }
                if right_ty == Type::Number {
                    // cast to int
                    right_ty = Type::Int;
                    self.cast(&mut right, &Type::Number, &Type::Int);
                }
                // both sides must be int or bigint
                if !(left_ty == Type::Int && right_ty == Type::Int)
                    && !(left_ty == Type::Bigint && right_ty == Type::Bigint)
                {
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // left should be equal to right
                debug_assert!(left_ty == right_ty);
                debug_assert!(left_ty == Type::Int || left_ty == Type::Bigint);

                // either bigint or int
                ty = left_ty;
            }
            swc::BinaryOp::EqEq
            | swc::BinaryOp::EqEqEq
            | swc::BinaryOp::NotEq
            | swc::BinaryOp::NotEqEq => {
                // result must be boolean
                ty = Type::Bool;
            }
            swc::BinaryOp::Gt | swc::BinaryOp::GtEq | swc::BinaryOp::Lt | swc::BinaryOp::LtEq => {
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Bool;
                // both are int
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    ty = Type::Bool;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    // cast left to number
                    left_ty = Type::Number;
                    self.cast(&mut left, &Type::Int, &Type::Number);
                    ty = Type::Bool;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    // cast right to number
                    right_ty = Type::Number;
                    self.cast(&mut right, &Type::Number, &Type::Int);
                    ty = Type::Bool;
                // both are bigint
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bool;
                } else {
                    // not bigint, number or int
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                debug_assert!(right_ty == left_ty);
            }
            swc::BinaryOp::NullishCoalescing | swc::BinaryOp::LogicalOr => {
                ty = left_ty.union(right_ty);
            }
            swc::BinaryOp::LogicalAnd => {
                if left_ty != Type::Bool {
                    self.cast(&mut left, &left_ty, &Type::Bool);
                    left_ty = Type::Bool;
                }
                if right_ty != Type::Bool {
                    self.cast(&mut right, &right_ty, &Type::Bool);
                    right_ty = Type::Bool;
                }

                debug_assert_eq!(right_ty, left_ty);
                debug_assert_eq!(right_ty, Type::Bool);

                ty = Type::Bool
            }
            swc::BinaryOp::In | swc::BinaryOp::InstanceOf => unreachable!(),
        };

        return Ok((
            Expr::Bin {
                op: b.op.into(),
                left: Box::new(left),
                right: Box::new(right),
            },
            ty,
        ));
    }
}
