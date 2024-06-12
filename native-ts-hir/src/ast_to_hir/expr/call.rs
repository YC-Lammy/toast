use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Callee, Expr, FuncType, Type};

use super::Result;

impl Transformer {
    /// translates the call expression.
    /// currently, generics are not supported yet
    pub fn translate_call_expr(&mut self, call: &swc::CallExpr) -> Result<(Expr, Type)> {
        let (callee, callee_ty) = match &call.callee {
            swc::Callee::Super(s) => {
                if !self.is_in_constructor {
                    return Err(Error::syntax_error(
                        s.span,
                        "super call is only allowed in constructors",
                    ));
                }

                let sup = self.super_class.expect("invalid super class");

                let constructor = &self
                    .context
                    .classes
                    .get(&sup)
                    .expect("invalid class")
                    .constructor;

                let func_ty = if let Some((_, ty)) = constructor {
                    ty.clone()
                } else {
                    FuncType {
                        this_ty: Type::Object(sup),
                        params: Vec::new(),
                        var_arg: false,
                        return_ty: Type::Undefined,
                    }
                };

                (Callee::Super(sup), func_ty.into())
            }
            swc::Callee::Import(i) => {
                return Err(Error::syntax_error(i.span, "dynamic import not allowed"))
            }
            swc::Callee::Expr(e) => {
                // translate the expression
                let (expr, ty) = self.translate_expr(e, None)?;

                // check it is a function
                let func_ty = if let Type::Function(func) = ty {
                    func
                } else {
                    return Err(Error::syntax_error(call.span, "callee is not a function"));
                };

                // convert to callee
                match expr {
                    Expr::Member {
                        span,
                        object,
                        key,
                        optional,
                    } => {
                        if optional {
                            return Err(Error::syntax_error(
                                call.callee.span(),
                                format!("Type '' is not callable",),
                            ));
                        }
                        // check if object matches func_ty.this type
                        // TODO
                        (
                            Callee::Member {
                                span: span,
                                object: *object,
                                prop: key,
                                optional: false,
                            },
                            func_ty,
                        )
                    }
                    Expr::Function(f) => {
                        // check this type matches
                        let this_ty = self.this_ty.clone();
                        self.type_check(call.span, &this_ty, &func_ty.this_ty)?;

                        (Callee::Function(f), func_ty)
                    }
                    _ => {
                        // check this type matches
                        let this_ty = self.this_ty.clone();
                        self.type_check(call.span, &this_ty, &func_ty.this_ty)?;

                        (Callee::Expr(expr), func_ty)
                    }
                }
            }
        };

        // type arguments
        if call.type_args.is_some() {
            todo!("generics")
        }

        // if it is not a member call, we have to check
        if !callee.is_member() {
            let this_ty = self.this_ty.clone();
            self.type_check(call.span, &this_ty, &callee_ty.this_ty)?;
        }

        // reference argument types
        let expected_arguments: &[Type] = &callee_ty.params;

        let mut args = Vec::new();

        // since var args is not supported, length of arguments is fixed
        if call.args.len() != callee_ty.params.len() {
            return Err(Error::syntax_error(
                call.span,
                format!(
                    "expected {} arguments, {} were given",
                    callee_ty.params.len(),
                    call.args.len()
                ),
            ));
        }

        // handle arguments
        for (i, arg) in call.args.iter().enumerate() {
            // spread ... is present
            if let Some(spread) = arg.spread {
                return Err(Error::syntax_error(
                    spread,
                    "variable arguments not supported",
                ));
            }

            // translate argument
            let (mut a, arg_ty) = self.translate_expr(&arg.expr, expected_arguments.get(i))?;

            // check argument fulfills type
            self.type_check(arg.span(), &arg_ty, &expected_arguments[i])?;

            if &arg_ty != &expected_arguments[i] {
                // convert value to type
                self.cast(&mut a, &arg_ty, &expected_arguments[i]);
            }

            // push expression to arguments
            args.push(a);
        }

        return Ok((
            Expr::Call {
                span: call.span,
                callee: Box::new(callee),
                args: args,
                optional: false,
            },
            callee_ty.return_ty.clone(),
        ));
    }
}
