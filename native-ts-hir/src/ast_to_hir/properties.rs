use std::sync::Arc;

use crate::{
    hir::{FuncType, ModuleValueExport, PropertyDesc, Type},
    PropName, Symbol,
};

use super::Transformer;

macro_rules! functy {
    (($this:expr; $($arg:expr),*)=>$re:expr) => {
        Type::Function(FuncType {
            this_ty: $this,
            params: vec![$($arg,)*],
            var_arg: false,
            return_ty: $re,
        }.into())
    };
}

impl Transformer {
    pub fn get_properties<'a>(&'a self, ty: &'a Type) -> &'a [PropertyDesc] {
        match ty {
            Type::Any | Type::AnyObject | Type::Null | Type::Undefined => &[],
            Type::LiteralObject(l) => &l,
            _ => todo!(),
        }
    }

    pub fn type_has_property(&self, ty: &Type, prop: &PropName, method: bool) -> Option<Type> {
        if let PropName::Ident(ident) = prop {
            if ident == "toString" {
                return Some(functy!((Type::Any;)=>Type::String));
            }
        }
        match ty {
            Type::Alias(_) => unreachable!(),
            Type::Any => None,
            Type::AnyObject => None,
            Type::Iterator(_) => {
                match prop {
                    PropName::Ident(ident) => match ident.as_str() {
                        "next" => todo!(),
                        _ => {}
                    },
                    _ => {}
                };
                return None;
            }
            Type::NamespaceObject(id) => {
                let (value, _ty, namespace) = self.module_export(*id, &prop);

                if let Some(value) = value {
                    match value {
                        ModuleValueExport::Function(f) => {
                            return Some(Type::Function(
                                self.context
                                    .functions
                                    .get(&f)
                                    .expect("invalid funcion")
                                    .ty()
                                    .into(),
                            ))
                        }
                        ModuleValueExport::GenericFunction(f) => todo!(),
                        ModuleValueExport::Var(_id, ty) => return Some(ty),
                    }
                }

                if let Some(namespace) = namespace {
                    return Some(Type::NamespaceObject(namespace));
                }

                return None;
            }
            Type::LiteralObject(obj) => {
                for p in obj.iter() {
                    if &p.name == prop {
                        return Some(p.ty.clone());
                    }
                }
                return None;
            }
            Type::Object(class) => {
                let class = self.context.classes.get(class).expect("invalid class");
                if method {
                    if let Some((_id, func_ty)) = class.methods.get(prop) {
                        return Some(Type::Function(Arc::new(func_ty.clone())));
                    }
                }
                if let Some(attr) = class.properties.get(prop) {
                    return Some(attr.ty.clone());
                }
                if let Some((_id, func_ty)) = class.methods.get(prop) {
                    return Some(Type::Function(Arc::new(func_ty.clone())));
                }
                return None;
            }
            Type::Interface(id) => {
                let iface = self.context.interfaces.get(id).expect("invalid interface");
                if method {
                    if let Some(m) = iface.methods.get(prop) {
                        return Some(Type::Function(Arc::new(FuncType {
                            this_ty: Type::Interface(*id),
                            params: m.params.clone(),
                            var_arg: false,
                            return_ty: m.return_ty.clone(),
                        })));
                    }
                }
                if let Some(attr) = iface.properties.get(prop) {
                    return Some(attr.ty.clone());
                }
                if let Some(m) = iface.methods.get(prop) {
                    return Some(Type::Function(Arc::new(FuncType {
                        this_ty: Type::Interface(*id),
                        params: m.params.clone(),
                        var_arg: false,
                        return_ty: m.return_ty.clone(),
                    })));
                }
                return None;
            }
            Type::Tuple(elems) => match prop {
                PropName::Int(index) => {
                    if *index >= elems.len() as i32 {
                        return None;
                    }
                    if *index < 0 {
                        return None;
                    }
                    return Some(elems[*index as usize].clone());
                }
                PropName::Ident(ident) => match ident.as_str() {
                    "length" => Some(Type::Int),
                    _ => None,
                },
                _ => None,
            },
            Type::Array(elem) => {
                match prop {
                    PropName::Int(_) => Some(elem.as_ref().clone()),
                    PropName::Private(_) | PropName::String(_) => None,
                    PropName::Ident(ident) => {
                        match ident.as_str() {
                            "length" => Some(Type::Int),
                            "at" => Some(
                                functy!((Type::Array(elem.clone()); Type::Int.union(Type::Undefined)) => Type::Undefined.union(elem.as_ref().clone())),
                            ),
                            "concat" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Array(elem.clone())],
                                var_arg: true,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "copyWithin" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Int,
                                    Type::Int,
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "entries" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(
                                    Type::Tuple([Type::Int, elem.as_ref().clone()].into()).into(),
                                ),
                            }))),
                            "every" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // the element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Bool,
                            }))),
                            "fill" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "filter" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "find" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "findIndex" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "findLast" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "findLastIndex" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "flat" => todo!(),
                            "flatMap" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Array(elem.clone()),
                                    })),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "forEach" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Undefined,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Undefined,
                            }))),
                            "includes" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Bool,
                            }))),
                            "indexOf" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "lastIndexOf" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "join" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::String.union(Type::Undefined)],
                                var_arg: false,
                                return_ty: Type::String,
                            }))),
                            "keys" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(Type::Int.into()),
                            }))),
                            "map" => todo!(),
                            "pop" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "push" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![elem.as_ref().clone()],
                                var_arg: true,
                                return_ty: Type::Int,
                            }))),
                            "reduce" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            elem.as_ref().clone(),
                                            elem.as_ref().clone(),
                                            Type::Int,
                                        ],
                                        var_arg: false,
                                        return_ty: elem.as_ref().clone(),
                                    })),
                                    elem.as_ref().clone().union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: elem.as_ref().clone(),
                            }))),
                            "reduceRight" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            elem.as_ref().clone(),
                                            elem.as_ref().clone(),
                                            Type::Int,
                                        ],
                                        var_arg: false,
                                        return_ty: elem.as_ref().clone(),
                                    })),
                                    elem.as_ref().clone().union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: elem.as_ref().clone(),
                            }))),
                            "reverse" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Undefined,
                            }))),
                            "shift" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "slice" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Int.union(Type::Undefined),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "some" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Arc::new(FuncType {
                                        this_ty: Type::Any,
                                        params: vec![
                                            // current element
                                            elem.as_ref().clone(),
                                            // index
                                            Type::Int,
                                            // this array
                                            Type::Array(elem.clone()),
                                        ],
                                        var_arg: false,
                                        return_ty: Type::Bool,
                                    })),
                                    // this arg
                                    Type::Any,
                                ],
                                var_arg: false,
                                return_ty: Type::Bool,
                            }))),
                            "sort" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Function(Arc::new(FuncType {
                                    this_ty: Type::Any,
                                    params: vec![elem.as_ref().clone(), elem.as_ref().clone()],
                                    var_arg: false,
                                    return_ty: Type::Int,
                                }))],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "splice" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // start
                                    Type::Int,
                                    // delete count
                                    Type::Int.union(Type::Undefined),
                                    // elements
                                    elem.as_ref().clone(),
                                ],
                                var_arg: true,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "toReverse" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "toSorted" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Function(Arc::new(FuncType {
                                    this_ty: Type::Any,
                                    params: vec![elem.as_ref().clone(), elem.as_ref().clone()],
                                    var_arg: false,
                                    return_ty: Type::Int,
                                }))],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "toSplice" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // start
                                    Type::Int,
                                    // delete count
                                    Type::Int.union(Type::Undefined),
                                    // elements
                                    elem.as_ref().clone(),
                                ],
                                var_arg: true,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "unshift" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![elem.as_ref().clone()],
                                var_arg: true,
                                return_ty: Type::Int,
                            }))),
                            "values" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(elem.clone()),
                            }))),
                            "with" => Some(Type::Function(Arc::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Int, elem.as_ref().clone()],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            _ => None,
                        }
                    }
                    PropName::Symbol(sym) => match sym {
                        &Symbol::Iterator => Some(Type::Iterator(elem.clone())),
                        Symbol::Unscopables => todo!(),
                        _ => None,
                    },
                }
            }
            _ => todo!(),
        }
    }
}
