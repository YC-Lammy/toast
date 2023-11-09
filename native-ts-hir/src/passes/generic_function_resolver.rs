use std::collections::HashMap;

use native_js_common::{error::Error, rc::Rc};
use swc_common::Span;

use crate::untyped_hir::{visit::{Visitor, BreakOrContinue, Visit}, Expr, Function, Type, Callee, DeepClone};


#[derive(Default)]
pub struct GenericFunctionResolver{}

impl GenericFunctionResolver{
    pub fn resolve_function_with_args(&mut self, sp:Span, func: &mut Function, type_args: &[Type]) -> Result<Option<Function>, Error<Span>>{
        if type_args.len() > func.ty.generics.len(){
            return Err(Error::syntax_error(sp, format!("function '{}' expected {} type arguments, {} were given", func.name, func.ty.generics.len(), type_args.len())))
        }

        // function is already concrete
        if func.ty.generics.len() == 0{
            return Ok(None)
        }

        let mut resolved_generics = HashMap::new();
        for (i, g) in func.ty.generics.iter().enumerate(){

            let ty =
            if let Some(arg) = type_args.get(i){
                resolved_generics.insert(g.id, arg.clone());
                arg
            } else{
                if let Some(d) = &g.default{
                    resolved_generics.insert(g.id, d.clone());
                    d
                } else{
                    return Err(Error::syntax_error(sp, format!("function '{}' expected {} type arguments, {} were given", func.name, func.ty.generics.len(), type_args.len())))
                }
            };

            // check for constrain
            if let Some(c) = &g.constrain{
                if let Type::Interface { span, type_args, interface } = c{
                    // interface should be resolved
                    debug_assert!(type_args.is_none());

                    // check if type fulfils interface
                    if !interface.check(ty){
                        return Err(Error::syntax_error(*span, format!("type argument '{:?}' does not fulfill interface '{}'", ty, interface.name)))
                    }
                } else{
                    // constrain must be interface
                    return Err(Error::syntax_error(g.span, "constrain of generic type must be interface"))
                }
            }
        };

        // generic replacer
        let mut replacer = super::GenericReplacer{
            generics: resolved_generics
        };

        // deep clone the function
        let mut fun = func.deep_clone();

        fun.ty.generics.clear();
        fun.visit(&mut replacer)?;

        return Ok(Some(fun))
    }
}

impl Visitor for GenericFunctionResolver{
    type Error = Error<Span>;
    const FINGER_PRINT: usize = 7;

    fn visit_expr(&mut self, expr: &mut crate::untyped_hir::Expr) -> Result<crate::untyped_hir::visit::BreakOrContinue, Self::Error> {
        match expr{
            // construct a function
            Expr::Function { 
                span, 
                type_args, 
                func 
            } => {
                let resolved = self.resolve_function_with_args(*span, func, type_args)?;

                // update the function
                if let Some(resolved) = resolved{
                    type_args.clear();
                    *func = Rc::new(resolved);
                }
            }
            // callee is a function
            Expr::Call { 
                span, 
                callee:Callee::Function(func), 
                is_optchain: _, // does not metter
                type_args, 
                args
            } => {
                // try resolving the function
                let resolved = self.resolve_function_with_args(*span, func, type_args)?;

                if let Some(resolved) = resolved{
                    // update the expression
                    *expr = Expr::Call { 
                        span: *span, 
                        callee: Callee::Function(Rc::new(resolved)), 
                        is_optchain: false, // we will set it to false
                        type_args: Vec::new(), 
                        args: args.clone()
                    };
                }
            }
            // callee is a static function
            Expr::Call { 
                span, 
                callee: Callee::ClassMember { 
                    span:_, 
                    class: Type::Class { 
                        span: _,
                        type_args: class_type_args, 
                        class 
                    }, 
                    prop, 
                    is_optchain:_ 
                }, 
                is_optchain: _, // if callee is fucntion, it will always return
                type_args, 
                args
            } => {
                // the class should be concrete
                debug_assert!(class_type_args.is_none());

                // try finding the static function
                if let Some(method) = class.static_functions.iter_mut().find(|m|m.name.eq(prop)){
                    let resolved = self.resolve_function_with_args(*span, &mut method.function, type_args)?;

                    // instead of replacing the function in class,
                    // we replace the call expression
                    if let Some(resolved) = resolved{
                        // update the expression
                        *expr = Expr::Call { 
                            span: *span, 
                            callee: Callee::Function(Rc::new(resolved)), 
                            is_optchain: false, // we will set it to false
                            type_args: Vec::new(), 
                            args: args.clone()
                        };
                    }

                } else{
                    // it is not a method, therefore generics does not apply
                    if !type_args.is_empty(){
                        return Err(Error::syntax_error(*span, "dynamic call cannot have type arguments"))
                    };
                }
            }
            Expr::Call { 
                span, 
                callee: Callee::Member { 
                    span:_, 
                    obj, 
                    prop, 
                    is_optchain: member_is_optchain 
                }, 
                is_optchain: call_is_optchain, 
                type_args, 
                args 
            } => {
                // if member is optchain
                if *member_is_optchain && !*call_is_optchain{
                    return Err(Error::syntax_error(*span, "cannot call on undefined"))
                }

                
            }
            // retrive a concrete static function
            Expr::ClassMember {
                span, 
                class: Type::Class { 
                    span: _,
                    type_args:class_type_args, 
                    class 
                }, 
                prop, 
                type_args, 
                is_optchain: _ // does not matter if it is a class method
            } => {
                // should be resolved
                debug_assert!(class_type_args.is_none());

                // find the function
                if let Some(method) = class.static_functions.iter_mut().find(|m|m.name.eq(prop)){
                    let resolved = self.resolve_function_with_args(*span, &mut method.function, type_args)?;

                    // instead of replacing the function in class,
                    // we replace the call expression
                    if let Some(resolved) = resolved{
                        *expr = Expr::Function { 
                            span: *span,
                            func: Rc::new(resolved),
                            type_args: Vec::new(),
                        };
                    }
                    
                } else{
                    // it is not a method, therefore generics does not apply
                    if !type_args.is_empty(){
                        return Err(Error::syntax_error(*span, "unexpected type arguments"))
                    };
                }
            }
            _ => {}
        }
        return Ok(BreakOrContinue::Continue)
    }
}