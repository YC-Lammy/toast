use std::collections::HashMap;

use native_js_common::error::Error;
use native_js_common::rc::Rc;
use swc_common::Span;

use crate::untyped_hir::visit::{Visitor, BreakOrContinue, Visit};
use crate::untyped_hir::{
    Type, DeepClone, InterfaceType
};

use super::GenericReplacer;

#[derive(Default)]
pub struct GenericInterfaceResolver{
    resolved:Vec<(*const InterfaceType, Box<[Type]>, Rc<InterfaceType>)>
}

impl Visitor for GenericInterfaceResolver{
    const FINGER_PRINT: usize = 8;
    type Error = Error<Span>;

    fn visit_type(&mut self, ty: &mut Type) -> Result<BreakOrContinue, Self::Error> {
        match ty{
            Type::Interface {
                span, 
                type_args, 
                interface
            } => {
                if type_args.len() > interface.generics.len(){
                    return Err(Error::syntax_error(*span, format!("interface '{}' expected {} type arguments, {} were given", interface.name, interface.generics.len(), type_args.len())))
                };

                if interface.generics.len() == 0{
                    return Ok(BreakOrContinue::Continue)
                };

                // already resolved
            if let Some((_, _, resolved)) = self.resolved.iter().find(|(base, args, _)|{
                *base == interface.as_ref() as *const _ && args == type_args
            }){
                *type_args = Box::new([]);
                *interface = resolved.clone();

                return Ok(BreakOrContinue::Continue)
            }

                let mut resolved_generics = HashMap::new();
                for (i, g) in interface.generics.iter().enumerate(){
                    let ty =
                    if let Some(ty) = type_args.get(i){
                        resolved_generics.insert(g.id, ty.clone());
                        ty
                    } else if let Some(ty) = &g.default{
                        resolved_generics.insert(g.id, ty.clone());
                        ty
                    } else{
                        return Err(Error::syntax_error(*span, format!("interface '{}' expected {} type arguments, {} were given", interface.name, interface.generics.len(), type_args.len())))
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

                // deep clone the interface
                let mut new_interface = interface.deep_clone();

                new_interface.generics.clear();
                new_interface.visit(&mut replacer)?;

                // replace type arguements
                *type_args = Box::new([]);
                // replace interface
                *interface = Rc::new(new_interface);
            }
            _ => {}
        }
        
        return Ok(BreakOrContinue::Continue)
    }
}


#[test]
fn test_generic_interface_resolver(){
    use crate::untyped_hir::{InterfaceType, GenericParam, GenericId, InterfaceProperty};

    let mut iface = Type::Interface { 
        span: Span::default(), 
        type_args: Box::new([Type::Any, Type::BigInt, Type::Int]), 
        interface: Rc::new(InterfaceType{
            name: "MyIface".to_string(),
            visited_fingerprint: 0,
            is_definite: true,
            extends: Vec::new(),
            generics: vec![
                GenericParam{
                    name: "A".into(),
                    span: Span::default(),
                    id: GenericId(0),
                    constrain: None,
                    default: None,
                },
                GenericParam{
                    name: "B".into(),
                    span: Span::default(),
                    id: GenericId(1),
                    constrain: None,
                    default: None,
                },
                GenericParam{
                    name: "C".into(),
                    span: Span::default(),
                    id: GenericId(2),
                    constrain: None,
                    default: None,
                },
            ],
            props: vec![
                InterfaceProperty{
                    name: crate::PropName::Ident("myfield".into()),
                    optinal: false,
                    readonly: false,
                    ty: Type::Union(vec![Type::Generic(GenericId(0)), Type::Generic(GenericId(1)), Type::Generic(GenericId(2))])
                }
            ],
            methods: vec![]
        })
    };

    let mut resolver = GenericInterfaceResolver::default();

    let re = resolver.visit_type(&mut iface);

    assert!(re.is_ok());

    if let Type::Interface { span:_, type_args, interface } = iface{
        assert!(type_args.is_empty());
        assert!(interface.is_definite);
        assert!(interface.generics.is_empty());
        assert!(interface.extends.is_empty());
        assert!(interface.methods.is_empty());
        assert!(interface.name == "MyIface");
        assert!(interface.props.len() == 1);
        assert!(interface.props[0].name == crate::PropName::Ident("myfield".into()));
        assert!(interface.props[0].ty == Type::Union(vec![Type::Any, Type::BigInt, Type::Int]))
    } else{
        panic!()
    }
}