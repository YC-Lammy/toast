use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::{Transformer, ValueBinding};
use crate::error::Error;
use crate::hir::{Callee, Expr, Type};

use super::Result;

impl Transformer {
    pub fn translate_optchain_expr(&mut self, n: &swc::OptChainExpr) -> Result<(Expr, Type)> {
        match n.base.as_ref() {
            swc::OptChainBase::Member(m) => {
                let (mut expr, ty) = self.translate_member_expr(m)?;
                if !n.optional {
                    return Ok((expr, ty));
                }

                // set optional to true
                if let Expr::Member { optional, .. } = &mut expr {
                    *optional = true;
                } else {
                    unreachable!()
                }

                return Ok((expr, ty.union(Type::Undefined)));
            }
            swc::OptChainBase::Call(c) => {
                let mut callee = None;
                let mut func_ty = None;

                let ty_args = if let Some(type_args) = &c.type_args {
                    let mut args = Vec::new();

                    for t in &type_args.params {
                        args.push(self.translate_type(&t)?)
                    }

                    args
                } else {
                    Vec::new()
                };

                // function call
                if let Some(ident) = c.callee.as_ident() {
                    // find the function
                    match self.context.find_value_binding(&ident.sym) {
                        // a function
                        Some(ValueBinding::Function(f)) => {
                            // type arguments not allowed
                            if !ty_args.is_empty() {
                                return Err(Error::syntax_error(
                                    c.type_args.as_ref().unwrap().span,
                                    "Type '' has no signatures for which the type argument list is applicable.",
                                ));
                            }

                            let id = *f;
                            let ty = self
                                .context
                                .functions
                                .get(&id)
                                .expect("invalid function")
                                .ty();

                            self.type_check(ident.span, &self.this_ty, &ty.this_ty)?;

                            callee = Some(Callee::Function(id));
                            func_ty = Some(ty);
                        }
                        // a generic function
                        Some(ValueBinding::GenericFunction(_id)) => {
                            todo!("generic function")
                        }
                        _ => {}
                    };
                }

                // expression
                if callee.is_none() {
                    // translate the callee as expression
                    let (expr, ty) = self.translate_expr(&c.callee, None)?;

                    let func = match &ty {
                        Type::Function(func_ty) => func_ty.as_ref().clone(),
                        Type::Union(u) => {
                            let mut func = None;

                            for ty in u.iter() {
                                match ty {
                                    Type::Null | Type::Undefined => {}
                                    Type::Function(f) => {
                                        if func.is_some() {
                                            return Err(Error::syntax_error(
                                                c.callee.span(),
                                                "type '' is not callable",
                                            ));
                                        }
                                        func = Some(f.as_ref().clone());
                                    }
                                    _ => {
                                        return Err(Error::syntax_error(
                                            c.callee.span(),
                                            "type '' is not callable",
                                        ))
                                    }
                                }
                            }

                            func.unwrap()
                        }
                        Type::Undefined => {
                            // will never call, return the expression
                            return Ok((expr, Type::Undefined));
                        }
                        Type::Null => {
                            // will never call, return the expression
                            return Ok((expr, Type::Null));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                c.callee.span(),
                                "type '' is not callable",
                            ))
                        }
                    };

                    // check this type matches
                    self.type_check(c.span, &self.this_ty, &func.this_ty)?;

                    // member call
                    if let Expr::Member {
                        object,
                        key,
                        optional,
                        span,
                    } = expr
                    {
                        callee = Some(Callee::Member {
                            span: span,
                            object: *object,
                            prop: key,
                            optional: optional,
                        });
                    } else {
                        callee = Some(Callee::Expr(expr));
                    }
                    func_ty = Some(func);
                };

                let callee = callee.unwrap();
                let func_ty = func_ty.unwrap();

                let mut args = Vec::new();
                let mut arg_tys = Vec::new();

                // translate arguments
                for (i, arg) in c.args.iter().enumerate() {
                    if let Some(spread) = arg.spread {
                        return Err(Error::syntax_error(
                            spread,
                            "variabl arguments not supported",
                        ));
                    }
                    let (expr, ty) = self.translate_expr(&arg.expr, func_ty.params.get(i))?;
                    args.push(expr);
                    arg_tys.push(ty);
                }

                // length must be the same
                if args.len() != func_ty.params.len() {
                    return Err(Error::syntax_error(
                        c.span,
                        format!(
                            "expected {} arguments, {} were given",
                            func_ty.params.len(),
                            args.len()
                        ),
                    ));
                }

                // return call expression
                return Ok((
                    Expr::Call {
                        span: n.span,
                        callee: Box::new(callee),
                        args: args,
                        optional: true,
                    },
                    func_ty.return_ty,
                ));
            }
        }
    }
}
