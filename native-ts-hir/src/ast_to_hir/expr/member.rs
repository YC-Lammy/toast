use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, Type};
use crate::PropName;

use super::Result;

impl Transformer {
    /// translate member expression
    pub fn translate_member_expr(&mut self, member: &swc::MemberExpr) -> Result<(Expr, Type)> {
        let prop = match &member.prop {
            swc::MemberProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::MemberProp::Ident(id) => {
                PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
            }
            swc::MemberProp::PrivateName(id) => {
                PropNameOrExpr::PropName(PropName::Private(id.id.sym.to_string()))
            }
        };

        let (obj, obj_ty) = self.translate_expr(&member.obj, None)?;

        if let PropNameOrExpr::PropName(PropName::Private(_)) = &prop {
            if self.this_ty != obj_ty {
                return Err(Error::syntax_error(
                    member.prop.span(),
                    "cannot access privite properties outside of method",
                ));
            }
        }

        match prop {
            PropNameOrExpr::PropName(name) => {
                if let Some(member_ty) = self.type_has_property(&obj_ty, &name, false) {
                    return Ok((
                        Expr::Member {
                            object: Box::new(obj),
                            key: PropNameOrExpr::PropName(name),
                            optional: false,
                        },
                        member_ty,
                    ));
                } else {
                    return Err(Error::syntax_error(
                        member.span,
                        format!("type has no property '{}'", name),
                    ));
                }
            }
            PropNameOrExpr::Expr(mut e, ty) => {
                match &obj_ty {
                    Type::Map(k, v) => {
                        self.type_check(member.span, &ty, k)?;

                        if &ty != k.as_ref() {
                            self.cast(&mut e, &ty, k.as_ref());
                        }
                        return Ok((
                            Expr::Member {
                                object: Box::new(obj),
                                key: PropNameOrExpr::Expr(e, ty),
                                optional: false,
                            },
                            v.as_ref().clone(),
                        ));
                    }
                    Type::Array(elem) => match ty {
                        Type::Int | Type::LiteralInt(_) | Type::Number | Type::LiteralNumber(_) => {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                elem.as_ref().clone(),
                            ));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                member.span,
                                "array can only be indexed by number",
                            ))
                        }
                    },
                    Type::Tuple(elems) => match ty {
                        Type::Int | Type::LiteralInt(_) | Type::Number | Type::LiteralNumber(_) => {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                Type::Union(elems.clone()),
                            ));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                member.span,
                                "tuple can only be indexed by number",
                            ));
                        }
                    },
                    _ => {
                        return Err(Error::syntax_error(
                            member.span,
                            "type '' is not indexable, property must be literal",
                        ))
                    }
                };
            }
        }
    }
}
