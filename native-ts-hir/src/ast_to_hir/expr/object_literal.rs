use native_ts_parser::swc_core::common::{Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::common::FunctionId;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, PropertyDesc, Type};
use crate::PropName;

use super::Result;

impl Transformer {
    /// translate object literal expression
    pub fn translate_object_lit_expr(
        &mut self,
        obj: &swc::ObjectLit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        let mut spans = Vec::new();
        let mut tys = Vec::new();
        let mut values = Vec::new();

        for p in &obj.props {
            // translate the property name
            let propname = match p {
                swc::PropOrSpread::Spread(s) => {
                    return Err(Error::syntax_error(
                        s.dot3_token,
                        "spread expression not supported",
                    ))
                }
                swc::PropOrSpread::Prop(p) => match p.as_ref() {
                    // multiple clause to reduce footprint
                    swc::Prop::Assign(a) => PropName::Ident(a.key.sym.to_string()),
                    swc::Prop::KeyValue(swc::KeyValueProp { key, .. })
                    | swc::Prop::Method(swc::MethodProp { key, .. })
                    | swc::Prop::Setter(swc::SetterProp { key, .. })
                    | swc::Prop::Getter(swc::GetterProp { key, .. }) => {
                        match self.translate_prop_name(&key)? {
                            PropNameOrExpr::PropName(p) => p,
                            PropNameOrExpr::Expr(_, _) => {
                                return Err(Error::syntax_error(
                                    key.span(),
                                    "computed property name not allowed",
                                ))
                            }
                        }
                    }
                    swc::Prop::Shorthand(s) => PropName::Ident(s.sym.to_string()),
                },
            };

            let expected_prop_ty = if let Some(expected) = expected_ty {
                if let Some(ty) = self.type_has_property(expected, &propname, false) {
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };

            // translate property
            match p {
                swc::PropOrSpread::Spread(_) => unreachable!(),
                swc::PropOrSpread::Prop(p) => {
                    match p.as_ref() {
                        swc::Prop::KeyValue(v) => {
                            // translate value
                            let (mut value, mut ty) =
                                self.translate_expr(&v.value, expected_prop_ty.as_ref())?;

                            // generalise type
                            ty = self.generalise_type(&mut value, &ty).unwrap_or(ty);

                            spans.push(v.key.span());
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Shorthand(s) => {
                            if s.optional {
                                return Err(Error::syntax_error(
                                    s.span,
                                    "object literal shorthand cannot be optional",
                                ));
                            }

                            // translate variale load
                            let (mut value, ty) = self.translate_var_load(s)?;

                            // type check
                            let ty = if let Some(expected) = expected_prop_ty {
                                self.type_check(s.span, &ty, &expected)?;
                                self.cast(&mut value, &ty, &expected);
                                // use expected type
                                expected
                            } else {
                                ty
                            };

                            spans.push(s.span);
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Method(m) => {
                            // create new function id
                            let funcid = FunctionId::new();
                            // tranlate function
                            self.translate_function(funcid, None, &m.function)?;

                            // create closure
                            let mut value = Expr::Closure(funcid);
                            // function type
                            let ty = Type::Function(
                                self.context.functions.get(&funcid).unwrap().ty().into(),
                            );

                            // type check
                            let ty = if let Some(expected) = expected_prop_ty {
                                self.type_check(m.key.span(), &ty, &expected)?;
                                self.cast(&mut value, &ty, &expected);
                                // use expected type
                                expected
                            } else {
                                ty
                            };

                            spans.push(m.key.span());
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Getter(g) => {
                            // todo: getter
                            return Err(Error::syntax_error(g.span, "getter not supported"));
                        }
                        swc::Prop::Setter(s) => {
                            // todo: setter
                            return Err(Error::syntax_error(s.span, "setter not supported"));
                        }
                        // this is invalid for object literal
                        swc::Prop::Assign(_) => unreachable!(),
                    }
                }
            }
        }

        // check for duplicated property names
        for (n, (p1, _)) in values.iter().enumerate() {
            for (i, (p2, _)) in values.iter().enumerate() {
                if n != i && p1 == p2 {
                    return Err(Error::syntax_error(spans[i], "duplicated property name"));
                }
            }
        }

        // add missing properties to the object
        if let Some(expected) = expected_ty {
            // loop through properties of expected type
            for p in self.get_properties(expected) {
                // missing property
                if values.iter().find(|(key, _)| key == &p.name).is_none() {
                    // undefined can be assigned to the property
                    if self.type_check(DUMMY_SP, &Type::Undefined, &p.ty).is_ok() {
                        // add property with value undefined
                        values.push((
                            p.name.clone(),
                            Expr::Cast {
                                span: DUMMY_SP,
                                value: Box::new(Expr::Undefined),
                                ty: p.ty.clone(),
                            },
                        ));
                        // add property to type
                        tys.push(p.clone());
                    }
                }
            }
        }

        // sort the descriptors
        tys.sort();

        // object expression
        let mut obj_expr = Expr::Object {
            span: obj.span,
            props: values,
        };
        let obj_ty = Type::LiteralObject(tys.into());

        // type check
        let ty = if let Some(expected) = expected_ty {
            self.type_check(obj.span, &obj_ty, &expected)?;
            self.cast(&mut obj_expr, &obj_ty, &expected);

            expected.clone()
        } else {
            obj_ty
        };

        return Ok((obj_expr, ty));
    }
}
