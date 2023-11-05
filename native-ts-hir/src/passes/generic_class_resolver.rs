use std::collections::HashMap;

use native_js_common::{error::Error, rc::Rc};
use swc_common::Span;

use crate::untyped_hir::{visit::{Visitor, BreakOrContinue, Visit}, Type, DeepClone, ClassType};

use super::GenericReplacer;

pub struct GenericClassResolver{
    resolved: Vec<(*const ClassType, Box<[Type]>, Rc<ClassType>)>
}

impl Visitor for GenericClassResolver{
    const FINGER_PRINT: usize = 9;
    type Error = Error<Span>;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        if let Type::Class { span, type_args, class } = ty{
            if type_args.len() > class.generics.len(){
                return Err(Error::syntax_error(*span, format!("class '{}' expected {} type arguments, {} were given", class.name, class.generics.len(), type_args.len())))
            };

            if class.generics.len() == 0{
                return Ok(BreakOrContinue::Continue)
            };

            // already resolved
            if let Some((_, _, resolved)) = self.resolved.iter().find(|(base, args, _)|{
                *base == class.as_ref() as *const _ && args == type_args
            }){
                *type_args = Box::new([]);
                *class = resolved.clone();

                return Ok(BreakOrContinue::Continue)
            }

            let mut resolved_generics = HashMap::new();
            for (i, g) in class.generics.iter().enumerate(){
                let ty =
                if let Some(ty) = type_args.get(i){
                    resolved_generics.insert(g.id, ty.clone());
                    ty
                } else if let Some(ty) = &g.default{
                    resolved_generics.insert(g.id, ty.clone());
                    ty
                } else{
                    return Err(Error::syntax_error(*span, format!("class '{}' expected {} type arguments, {} were given", class.name, class.generics.len(), type_args.len())))
                };

                if let Some(constrain) = &g.constrain{
                    if let Type::Interface { span, type_args, interface } = constrain{
                        debug_assert!(type_args.is_empty());

                        if !interface.check(ty){
                            return Err(Error::syntax_error(*span, format!("type argument '{:?}' does not fulfill interface '{}'", ty, interface.name)))
                        }
                    } else{
                        return Err(Error::syntax_error(g.span, "constrain of generic type must be interface"))
                    }
                }
            };


            let mut replacer = GenericReplacer{
                generics: resolved_generics
            };

            let mut new_class = class.deep_clone();

            new_class.generics.clear();
            new_class.visit(&mut replacer)?;
            
            let new_class = Rc::new(new_class);
            
            let type_args = core::mem::replace(type_args, Box::new([]));

            self.resolved.push((class.as_ref(), type_args, new_class.clone()));

            *class = new_class.clone();
        }
        return Ok(BreakOrContinue::Continue)
    }
}