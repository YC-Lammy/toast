use std::collections::HashMap;

use native_js_common::{error::Error, rc::Rc};
use swc_common::Span;

use crate::untyped_hir::{
    visit::{BreakOrContinue, Visit, Visitor},
    ClassType, DeepClone, Type,
};

use super::GenericReplacer;

pub struct GenericClassResolver {
    resolved: Vec<(*const ClassType, Box<[Type]>, Rc<ClassType>)>,
}

impl Visitor for GenericClassResolver {
    const FINGER_PRINT: usize = 9;
    type Error = Error<Span>;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        if let Type::Class {
            span,
            type_args,
            class,
        } = ty
        {
            let len = type_args.as_ref().map(|a| a.len()).unwrap_or(0);

            if len > class.generics.len() {
                return Err(Error::syntax_error(
                    *span,
                    format!(
                        "class '{}' expected {} type arguments, {} were given",
                        class.name,
                        class.generics.len(),
                        len
                    ),
                ));
            };

            if class.generics.len() == 0 {
                return Ok(BreakOrContinue::Continue);
            };

            let ty_args = type_args.as_mut().map(|b| b.as_mut()).unwrap_or(&mut []);

            // already resolved
            if let Some((_, _, resolved)) = self.resolved.iter().find(|(base, args, _)| {
                *base == class.as_ref() as *const _ && args.as_ref() == ty_args
            }) {
                *type_args = None;
                *class = resolved.clone();

                return Ok(BreakOrContinue::Continue);
            }

            let mut resolved_generics = HashMap::new();
            for (i, g) in class.generics.iter().enumerate() {
                let ty = if let Some(ty) = ty_args.get(i) {
                    resolved_generics.insert(g.id, ty.clone());
                    ty
                } else if let Some(ty) = &g.default {
                    resolved_generics.insert(g.id, ty.clone());
                    ty
                } else {
                    return Err(Error::syntax_error(
                        *span,
                        format!(
                            "class '{}' expected {} type arguments, {} were given",
                            class.name,
                            class.generics.len(),
                            ty_args.len()
                        ),
                    ));
                };

                if let Some(constrain) = &g.constrain {
                    if let Type::Interface {
                        span,
                        type_args,
                        interface,
                    } = constrain
                    {
                        debug_assert!(type_args.is_none());

                        if !interface.check(ty) {
                            return Err(Error::syntax_error(
                                *span,
                                format!(
                                    "type argument '{:?}' does not fulfill interface '{}'",
                                    ty, interface.name
                                ),
                            ));
                        }
                    } else {
                        return Err(Error::syntax_error(
                            g.span,
                            "constrain of generic type must be interface",
                        ));
                    }
                }
            }

            let mut replacer = GenericReplacer {
                generics: resolved_generics,
            };

            let mut new_class = class.deep_clone();

            new_class.generics.clear();
            new_class.visit(&mut replacer)?;

            let new_class = Rc::new(new_class);

            self.resolved.push((
                class.as_ref(),
                ty_args.to_vec().into_boxed_slice(),
                new_class.clone(),
            ));

            *type_args = None;
            *class = new_class.clone();
        }
        return Ok(BreakOrContinue::Continue);
    }
}
