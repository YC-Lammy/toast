use native_ts_parser::swc_core::{common::Spanned, ecma::ast as swc};

use crate::{
    ast_to_hir::Transformer,
    error::Error,
    hir::{Expr, Type},
};

use super::Result;

impl Transformer {
    /// array construction expression
    pub fn translate_array_expr(
        &mut self,
        a: &swc::ArrayLit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        // we translate the array expression according to the expected type
        match expected_ty {
            // expecting an array
            Some(Type::Array(expected_elem_ty)) => {
                // elements in the array
                let mut elements = Vec::new();

                // loop through elements
                for elem in &a.elems {
                    // only translates it when it is not empty
                    let (span, mut expr, ty) = if let Some(elem) = elem {
                        // spread ... is not supported
                        // todo: spread in array expression
                        if let Some(spread) = elem.spread {
                            return Err(Error::syntax_error(
                                spread,
                                "spread expression not allowed",
                            ));
                        }

                        // translate the element expression
                        let (e, t) = self.translate_expr(&elem.expr, Some(expected_elem_ty))?;
                        // return translated element
                        (elem.expr.span(), e, t)
                    } else {
                        // element is empty, it is an undefined
                        (a.span, Expr::Undefined, Type::Undefined)
                    };

                    // check the element expression matchs expected element type
                    self.type_check(span, &ty, &expected_elem_ty)?;

                    // if the element type is not the same, cast
                    if !expected_elem_ty.as_ref().eq(&ty) {
                        self.cast(&mut expr, &ty, expected_elem_ty);
                    }

                    // push element to array
                    elements.push(expr);
                }

                // return array expression
                return Ok((
                    Expr::Array {
                        span: a.span,
                        values: elements,
                    },
                    Type::Array(expected_elem_ty.clone()),
                ));
            }
            // expecting a tuple
            Some(Type::Tuple(element_tys)) => {
                // length of tuple is fixed and must be the same
                if element_tys.len() != a.elems.len() {
                    // return error if length does not match
                    return Err(Error::syntax_error(
                        a.span,
                        format!(
                            "expected {} elements, {} were given",
                            element_tys.len(),
                            a.elems.len()
                        ),
                    ));
                }

                // elements in tuple
                let mut values = Vec::with_capacity(element_tys.len());

                // loop through elements
                for (i, elem) in a.elems.iter().enumerate() {
                    // get the expected element type at index
                    let expected = element_tys.get(i).unwrap();

                    // translate the element if not empty
                    let (_span, expr, _ty) = if let Some(elem) = elem {
                        // spread expression not allowed
                        // todo: spread in array expression
                        if let Some(spread) = elem.spread {
                            return Err(Error::syntax_error(
                                spread,
                                "spread expression not allowed",
                            ));
                        }
                        // translate element expression
                        let (e, t) = self.translate_expr(&elem.expr, Some(expected))?;

                        // return translated element
                        (elem.span(), e, t)
                    } else {
                        // element is empty, it is undefined
                        // check if undefined satisfies expected element type
                        self.type_check(a.span, &Type::Undefined, expected)?;

                        // return undefined
                        (a.span, Expr::Undefined, Type::Undefined)
                    };

                    // push expression to tuple
                    values.push(expr);
                }

                // return the tuple expression
                return Ok((
                    Expr::Tuple {
                        span: a.span,
                        values: values,
                    },
                    Type::Tuple(element_tys.clone()),
                ));
            }
            // not tuple or array
            _ => {
                // the common type of expressions
                // this will be used if no expected type
                let mut ty: Option<Type> = None;
                // the values
                let mut values = Vec::new();

                // loop through elements
                for elem in &a.elems {
                    // translate the element if not empty
                    let (mut expr, mut t) = if let Some(elem) = elem {
                        // translate the element expression
                        let (expr, t) = self.translate_expr(&elem.expr, None)?;
                        // return the expression
                        (expr, t)
                    } else {
                        // element is empty, it is undefined
                        (Expr::Undefined, Type::Undefined)
                    };

                    // match the type of value
                    match t {
                        // int must be casted to float
                        Type::Int | Type::LiteralInt(_) => {
                            t = Type::Number;
                            // cast expression to number
                            self.cast(&mut expr, &Type::Int, &Type::Number);
                        }
                        Type::LiteralNumber(_) => {
                            t = Type::Number;
                        }
                        Type::LiteralBigint(_) => {
                            t = Type::Bigint;
                        }
                        Type::LiteralBool(_) => {
                            t = Type::Bool;
                        }
                        Type::LiteralString(_) => t = Type::String,
                        _ => {}
                    };

                    // construct a union type for elements
                    if let Some(chained) = ty {
                        ty = Some(chained.union(t));
                    } else {
                        ty = Some(t);
                    };
                    // push element to array
                    values.push(expr);
                }

                // construct an array type
                let array_ty = ty
                    .map(|t| Type::Array(t.into()))
                    .or_else(|| Some(Type::Array(Type::Any.into())))
                    .unwrap();

                // return the array expression
                return Ok((
                    Expr::Array {
                        span: a.span,
                        values: values,
                    },
                    array_ty,
                ));
            }
        }
    }
}
