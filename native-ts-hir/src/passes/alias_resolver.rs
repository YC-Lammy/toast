use std::collections::HashMap;

use native_js_common::error::Error;
use native_js_common::rc::Rc;
use swc_common::Span;

use crate::untyped_hir::{
    visit::{BreakOrContinue, Visit, Visitor},
    Type,
};

use super::GenericReplacer;

/// the alias resolver normalises alias types
pub struct AliasResolver;

impl Visitor for AliasResolver {
    type Error = Error<Span>;
    const FINGER_PRINT: usize = 3;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        if let Type::Alias {
            span,
            type_args,
            alias,
        } = ty
        {
            let mut resolved_generics = HashMap::new();

            let type_args = if let Some(type_args) = type_args {
                type_args.as_mut()
            } else {
                &mut []
            };

            // number of type arguments must be less then generics
            if type_args.len() > alias.generics.len() {
                return Err(Error::syntax_error(
                    *span,
                    format!(
                        "alias type '{}' expected {} type arguments, {} were given",
                        alias.name,
                        alias.generics.len(),
                        type_args.len()
                    ),
                ));
            }

            // an alias without generics is just its base type
            if alias.generics.len() == 0 {
                *ty = alias.base.clone();
                return Ok(BreakOrContinue::Continue);
            }

            // map all the generic types
            for (i, g) in alias.generics.iter().enumerate() {
                // try to resolve generic from param
                if let Some(ty) = type_args.get(i) {
                    // insert type to map
                    resolved_generics.insert(g.id, ty.clone());
                } else {
                    // try to resolve generic with default type
                    if let Some(default) = &g.default {
                        resolved_generics.insert(g.id, default.clone());
                    } else {
                        // neither argument nor default present
                        return Err(Error::syntax_error(
                            *span,
                            format!(
                                "alias type '{}' expected {} type arguments, {} were given",
                                alias.name,
                                alias.generics.len(),
                                type_args.len()
                            ),
                        ));
                    }
                }
            }

            // construct the replacer
            let mut replacer = GenericReplacer {
                generics: resolved_generics,
            };

            // clone the base type
            let mut base = alias.base.clone();

            // replace the solved generics
            match &mut base {
                Type::Class {
                    span: _,
                    type_args,
                    class: _,
                } => {
                    // replace the type arguments
                    if let Some(type_args) = type_args {
                        for arg in type_args.iter_mut() {
                            arg.visit(&mut replacer)?;
                        }
                    }
                }
                Type::Function { type_args, func } => {
                    // replace the type arguments
                    if let Some(type_args) = type_args {
                        for arg in type_args.iter_mut() {
                            arg.visit(&mut replacer)?;
                        }
                    }
                    // the function type must be deep cloned
                    let mut func_ty = func.as_ref().clone();
                    func_ty.visit(&mut replacer)?;
                    *func = Rc::new(func_ty);
                }
                Type::Union(u) => {
                    // replace variants
                    for t in u {
                        t.visit(&mut replacer)?;
                    }
                }
                Type::Interface {
                    span: _,
                    type_args,
                    interface: _,
                } => {
                    // replace the type arguments
                    if let Some(type_args) = type_args {
                        for arg in type_args.iter_mut() {
                            arg.visit(&mut replacer)?;
                        }
                    }
                }
                _ => return Err(Error::syntax_error(*span, "type expected 0 type arguments")),
            };

            // replace self with the base type
            *ty = base;
        }

        return Ok(BreakOrContinue::Continue);
    }
}
