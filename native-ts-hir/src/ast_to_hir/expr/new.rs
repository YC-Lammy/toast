use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use super::Result;
use crate::ast_to_hir::{ClassBinding, Transformer};
use crate::error::Error;
use crate::hir::{Expr, Type};

impl Transformer {
    pub fn translate_new_expr(&mut self, n: &swc::NewExpr) -> Result<(Expr, Type)> {
        let ident = match n.callee.as_ident() {
            Some(ident) => ident,
            None => return Err(Error::syntax_error(n.callee.span(), "expected identifier")),
        };

        let mut arguments = Vec::new();

        match self.context.find_class_binding(&ident.sym) {
            Some(ClassBinding::Class(class_id)) => {
                // should have no type arguments
                if let Some(args) = &n.type_args {
                    return Err(Error::syntax_error(args.span, "expected 0 type arguments"));
                }

                let class_id = *class_id;

                let c = self.context.classes.get(&class_id).expect("invalid class");

                if let Some((_const_id, const_ty)) = &c.constructor {
                    let params = const_ty.params.clone();

                    if let Some(args) = &n.args {
                        if args.len() != const_ty.params.len() {
                            return Err(Error::syntax_error(
                                n.span,
                                format!(
                                    "expected {} arguments, {} were given",
                                    const_ty.params.len(),
                                    args.len()
                                ),
                            ));
                        }

                        for (i, arg) in args.iter().enumerate() {
                            if let Some(spread) = arg.spread {
                                return Err(Error::syntax_error(
                                    spread,
                                    "spread argument is not supported",
                                ));
                            }

                            let (arg, _) = self.translate_expr(&arg.expr, params.get(i))?;
                            arguments.push(arg);
                        }
                    } else {
                        if const_ty.params.len() != 0 {
                            return Err(Error::syntax_error(
                                n.span,
                                format!("expected {} arguments", const_ty.params.len()),
                            ));
                        }
                    };
                } else {
                    if n.args.as_ref().is_some_and(|a| a.len() != 0) {
                        return Err(Error::syntax_error(n.span, "expected 0 arguments"));
                    }
                };

                return Ok((
                    Expr::New {
                        span: n.span,
                        class: class_id,
                        args: arguments,
                    },
                    Type::Object(class_id),
                ));
            }
            Some(ClassBinding::Generic(_class_id)) => {
                todo!("generic class")
            }
            None => {
                if self.context.has_binding(&ident.sym) {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("'{}' is not a constructor.", ident.sym),
                    ));
                } else {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("undefined identifier '{}'", ident.sym),
                    ));
                }
            }
        };
    }
}
