use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, Type};
use crate::PropName;

use super::Result;

impl Transformer {
    pub fn translate_super_prop_expr(&mut self, s: &swc::SuperPropExpr) -> Result<(Expr, Type)> {
        if self.super_class.is_none() {
            return Err(Error::syntax_error(
                s.span,
                "'super' keyword unexpected here",
            ));
        }

        let prop = match &s.prop {
            swc::SuperProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::SuperProp::Ident(id) => {
                PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
            }
        };

        let prop = match prop {
            PropNameOrExpr::PropName(p) => p,
            PropNameOrExpr::Expr(..) => {
                return Err(Error::syntax_error(
                    s.span,
                    "super property must be literal",
                ))
            }
        };

        let super_class = self.super_class.unwrap();

        // if in constructor, super means the class itself
        if self.is_in_constructor {
            // get the class
            if let Some(cl) = self.context.classes.get(&super_class) {
                // find static property
                if let Some((vid, ty)) = cl.static_properties.get(&prop) {
                    return Ok((
                        Expr::VarLoad {
                            span: s.span,
                            variable: *vid,
                        },
                        ty.clone(),
                    ));
                }
                // find static functions
                if let Some((fid, ty)) = cl.static_methods.get(&prop) {
                    return Ok((Expr::Function(*fid), Type::Function(ty.clone().into())));
                }

                if let Some(_) = cl.static_generic_methods.get(&prop) {
                    return Err(Error::syntax_error(s.span, "missing type arguments"));
                }
            } else {
                // the class should be defined
                unreachable!()
            }

            // the super class has no static property
            return Err(Error::syntax_error(
                s.span,
                format!("super has no property '{}'", prop),
            ));
        }

        // context is in method
        if let Some(ty) = self.type_has_property(&Type::Object(super_class), &prop, false) {
            // return member expression
            return Ok((
                Expr::Member {
                    span: s.span,
                    object: Box::new(
                        // cast this to super
                        Expr::Cast {
                            span: s.obj.span,
                            value: Box::new(Expr::This(s.obj.span)),
                            ty: Type::Object(super_class),
                        },
                    ),
                    key: PropNameOrExpr::PropName(prop),
                    optional: false,
                },
                ty,
            ));
        }

        return Err(Error::syntax_error(
            s.span,
            format!("super has no property '{}'", prop),
        ));
    }
}
