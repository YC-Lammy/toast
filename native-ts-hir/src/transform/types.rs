use std::collections::HashMap;

use native_js_common::error::Error;
use native_ts_parser::swc_core::common::{Span, Spanned};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast::{
    EnumType, EnumVariantDesc, FuncType, InterfaceMethod, InterfacePropertyDesc, InterfaceType,
    PropNameOrExpr, Type,
};
use crate::common::{AliasId, ClassId, FunctionId, InterfaceId};
use crate::{PropName, Symbol};

type Result<T> = std::result::Result<T, Error<Span>>;

use super::{context::Binding, Transformer};

impl Transformer {
    /// translate a function type without translating its contents
    pub fn translate_function_ty(&mut self, func: &swc::Function) -> Result<FuncType> {
        // generic function
        if func.type_params.is_some() {
            todo!("generic function")
        }

        // the default 'this' type
        let mut this_ty = Type::Any;
        // the default return type
        let mut return_ty = Type::Undefined;
        // stores the params
        let mut params = Vec::new();
        // is variable argument
        let is_var_arg = false;

        // loop through params
        for (i, p) in func.params.iter().enumerate() {
            // only support ident as param
            if let Some(ident) = p.pat.as_ident() {
                // translate param type
                if let Some(ann) = &ident.type_ann {
                    // translate the type
                    let ty = self.translate_type(&ann.type_ann)?;

                    // if ident is 'this' and is first, this type is set
                    if i == 0 && ident.sym.as_ref() == "this" {
                        this_ty = ty;
                    } else {
                        // push param type
                        params.push(ty);
                    };
                } else {
                    // function param must have type annotation
                    return Err(Error::syntax_error(ident.span, "missing type annotation"));
                }
            } else if let Some(rest) = p.pat.as_rest() {
                // variable agument

                // variable argument can only be declared at the last param
                if i != func.params.len() - 1 {
                    return Err(Error::syntax_error(
                        rest.dot3_token,
                        "variable arguments is only allowed at the last param",
                    ));
                }

                // TODO: variable argument
                return Err(Error::syntax_error(
                    rest.dot3_token,
                    "variable arguments not supported",
                ));
            } else {
                // we currently only supports ident params
                return Err(Error::syntax_error(
                    p.span,
                    "destructive params not allowed",
                ));
            }
        }

        // translate the return type if any
        if let Some(ann) = &func.return_type {
            return_ty = self.translate_type(&ann.type_ann)?;
        }

        // set function to async if declared
        if func.is_async {
            return_ty = Type::Promise(Box::new(return_ty));
        }

        // set function to generator if declared
        if func.is_generator {
            return_ty = Type::Iterator(Box::new(return_ty));
        }

        // return the function type
        return Ok(FuncType {
            this_ty,
            params,
            var_arg: is_var_arg,
            return_ty,
        });
    }

    /// translate an interface type for an interface declare
    pub fn translate_interface(&mut self, iface: &swc::TsInterfaceDecl) -> Result<InterfaceType> {
        // interface type body
        let mut iface_ty = InterfaceType {
            name: iface.id.sym.to_string(),
            extends: Vec::new(),
            implements: Vec::new(),
            properties: HashMap::new(),
            methods: HashMap::new(),
        };

        // translate constrains
        for ty in &iface.extends {
            // translate type arguments
            let type_args = if let Some(type_args) = &ty.type_args {
                self.translate_type_args(&type_args)?
            } else {
                // no type arguments
                Vec::new()
            };

            // translate extended type
            let t = self.translate_expr_type(&ty.expr, &type_args)?;

            match t {
                // a class
                Type::Object(class_id) => iface_ty.extends.push(class_id),
                // an interface
                Type::Interface(iface_id) => iface_ty.implements.push(iface_id),
                // other types are not allowed
                _ => {
                    return Err(Error::syntax_error(
                        ty.span,
                        format!("expected class or interface, found type '{:?}'", t),
                    ))
                }
            }
        }

        // generic interface
        if let Some(_) = &iface.type_params {
            panic!("generic interface")
        }

        // translate interface body
        for elem in &iface.body.body {
            match elem {
                swc::TsTypeElement::TsCallSignatureDecl(c) => {
                    // TODO: call signature declare
                    return Err(Error::syntax_error(c.span, "call signature not allowed"));
                }
                swc::TsTypeElement::TsConstructSignatureDecl(c) => {
                    // TODO: construct signature declare
                    return Err(Error::syntax_error(
                        c.span,
                        "constructor signature not allowed",
                    ));
                }
                swc::TsTypeElement::TsIndexSignature(i) => {
                    // TODO: index signature
                    return Err(Error::syntax_error(i.span, "index signature not allowed"));
                }
                swc::TsTypeElement::TsGetterSignature(g) => {
                    // TODO: getter
                    return Err(Error::syntax_error(g.span, "getter not supported"));
                }
                swc::TsTypeElement::TsSetterSignature(s) => {
                    // TODO: setter
                    return Err(Error::syntax_error(s.span, "setter not supported"));
                }
                swc::TsTypeElement::TsPropertySignature(p) => {
                    // initialiser not allowed in interface
                    if let Some(init) = &p.init {
                        return Err(Error::syntax_error(init.span(), "initialiser not allowed"));
                    }
                    //
                    if let Some(type_params) = &p.type_params {
                        return Err(Error::syntax_error(
                            type_params.span,
                            "generics not allowed",
                        ));
                    }
                    //
                    if !p.params.is_empty() {
                        return Err(Error::syntax_error(p.span, "params not allowed"));
                    }
                    // translate property name
                    let key = if p.computed {
                        // computed property name
                        self.translate_computed_prop_name(&p.key)?
                    } else {
                        // identifier property name
                        if let Some(id) = p.key.as_ident() {
                            PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
                        } else {
                            self.translate_computed_prop_name(&p.key)?
                        }
                    };

                    // match property name
                    let key = match key {
                        PropNameOrExpr::Expr(..) => {
                            // property must be known at compile time
                            return Err(Error::syntax_error(
                                p.key.span(),
                                "property of interface must be literal",
                            ));
                        }
                        // a property name
                        PropNameOrExpr::PropName(p) => p,
                    };

                    // check if property name is already declared
                    if iface_ty.properties.contains_key(&key) {
                        return Err(Error::syntax_error(p.span, "duplicated attributes"));
                    }

                    // translate type annotation
                    if let Some(ann) = &p.type_ann {
                        let mut ty = self.translate_type(&ann.type_ann)?;

                        // optional type
                        if p.optional {
                            ty = ty.union(Type::Undefined);
                        }

                        // insert property
                        iface_ty.properties.insert(
                            key,
                            InterfacePropertyDesc {
                                ty: ty,
                                readonly: p.readonly,
                                optional: p.optional,
                            },
                        );
                    } else {
                        // property must be annotated
                        return Err(Error::syntax_error(p.span, "missing type annotation"));
                    }
                }
                // an interface method
                swc::TsTypeElement::TsMethodSignature(m) => {
                    // generic method
                    if let Some(type_params) = &m.type_params {
                        return Err(Error::syntax_error(
                            type_params.span,
                            "generics not allowed",
                        ));
                    }

                    // the method body
                    let mut method_ty = InterfaceMethod {
                        readonly: m.readonly,
                        optional: m.optional,
                        params: Vec::new(),
                        return_ty: Type::Undefined,
                    };

                    // method name
                    let key = if m.computed {
                        self.translate_computed_prop_name(&m.key)?
                    } else {
                        // ident name
                        if let Some(id) = m.key.as_ident() {
                            PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
                        } else {
                            self.translate_computed_prop_name(&m.key)?
                        }
                    };

                    // match key
                    let key = match key {
                        PropNameOrExpr::Expr(..) => {
                            // dynamic names are not allowed
                            return Err(Error::syntax_error(
                                m.key.span(),
                                "property of interface must be literal",
                            ));
                        }
                        PropNameOrExpr::PropName(p) => p,
                    };

                    // check if method already declared
                    if iface_ty.methods.contains_key(&key) {
                        return Err(Error::syntax_error(m.span, "duplicated methods"));
                    }

                    // translate parameters
                    for param in &m.params {
                        match param {
                            swc::TsFnParam::Ident(ident) => {
                                if let Some(ann) = &ident.type_ann {
                                    let mut ty = self.translate_type(&ann.type_ann)?;

                                    if ident.optional {
                                        ty = ty.union(Type::Undefined);
                                    }
                                    method_ty.params.push(ty);
                                }
                            }
                            _ => {
                                return Err(Error::syntax_error(
                                    param.span(),
                                    "destructive params not allowed",
                                ))
                            }
                        }
                    }

                    if let Some(ann) = &m.type_ann {
                        let ty = self.translate_type(&ann.type_ann)?;
                        method_ty.return_ty = ty;
                    }

                    iface_ty.methods.insert(key, method_ty);
                }
            }
        }

        return Ok(iface_ty);
    }

    pub fn translate_enum(&mut self, e: &swc::TsEnumDecl) -> Result<EnumType> {
        let mut variants = Vec::new();

        for m in &e.members {
            let name = match &m.id {
                swc::TsEnumMemberId::Ident(id) => PropName::Ident(id.sym.to_string()),
                swc::TsEnumMemberId::Str(s) => PropName::String(s.value.to_string()),
            };
            variants.push(EnumVariantDesc { name: name });
        }

        Ok(EnumType {
            name: e.id.sym.to_string(),
            variants: variants,
        })
    }

    pub fn translate_type_alias(&mut self, alias: &swc::TsTypeAliasDecl) -> Result<Type> {
        if alias.type_params.is_none() {
            return self.translate_type(&alias.type_ann);
        }

        todo!()
    }

    pub fn translate_expr_type(&mut self, expr: &swc::Expr, type_args: &[Type]) -> Result<Type> {
        let binding = match self.find_expr_binding(&expr) {
            Some(b) => b,
            None => return Err(Error::syntax_error(expr.span(), "undefined identifier")),
        };

        let mut generics_allowed = false;

        let ty = match binding {
            Binding::Class(c) => Type::Object(c),
            Binding::Enum(e) => Type::Enum(e),
            Binding::Interface(i) => Type::Interface(i),
            Binding::TypeAlias(id) => {
                if let Some(ty) = self.context.alias.get(&id) {
                    ty.clone()
                } else {
                    Type::Alias(id)
                }
            }
            Binding::GenericFunction(_) | Binding::Function(_) => {
                return Err(Error::syntax_error(
                    expr.span(),
                    "expected type, found function",
                ))
            }
            Binding::Generic(_) => todo!("generics"),
            Binding::GenericClass(id) => {
                generics_allowed = true;

                if type_args.len() == 0 {
                    return Err(Error::syntax_error(expr.span(), "missing type arguments"));
                }
                let id = self.solve_generic_class(id, type_args)?;
                Type::Object(id)
            }

            Binding::GenericInterface(id) => {
                generics_allowed = true;

                if type_args.is_empty() {
                    return Err(Error::syntax_error(expr.span(), "missing type arguments"));
                }

                let id = self.solve_generic_interface(id, type_args)?;
                Type::Interface(id)
            }
            Binding::GenericTypeAlias(id) => {
                generics_allowed = true;

                if type_args.is_empty() {
                    return Err(Error::syntax_error(expr.span(), "missing type arguments"));
                }

                let id = self.solve_generic_alias(id, type_args)?;

                if let Some(ty) = self.context.alias.get(&id) {
                    ty.clone()
                } else {
                    Type::Alias(id)
                }
            }
            Binding::NameSpace(_) => {
                return Err(Error::syntax_error(
                    expr.span(),
                    "expected type, found namespace",
                ))
            }
            Binding::Using { .. } | Binding::Var { .. } => {
                return Err(Error::syntax_error(
                    expr.span(),
                    "expected type, found variable",
                ))
            }
        };

        if !generics_allowed {
            if !type_args.is_empty() {
                return Err(Error::syntax_error(
                    expr.span(),
                    "expected 0 type arguments",
                ));
            }
        }

        return Ok(ty);
    }

    fn find_expr_binding(&mut self, expr: &swc::Expr) -> Option<Binding> {
        match expr {
            swc::Expr::Paren(p) => return self.find_expr_binding(&p.expr),
            swc::Expr::Member(m) => {
                let binding = self.find_expr_binding(&m.obj)?;

                match binding {
                    Binding::NameSpace(mid) => {
                        let prop = match &m.prop {
                            swc::MemberProp::Computed(_) => return None,
                            swc::MemberProp::Ident(id) => PropName::Ident(id.to_string()),
                            swc::MemberProp::PrivateName(_) => return None,
                        };
                        return self.find_binding_from_module(mid, &prop);
                    }
                    _ => return None,
                }
            }
            swc::Expr::Ident(id) => return self.context.find(&id.sym).cloned(),
            _ => None,
        }
    }

    pub fn translate_type(&mut self, ty: &swc::TsType) -> Result<Type> {
        match ty {
            swc::TsType::TsArrayType(array) => {
                let member = self.translate_type(&array.elem_type)?;
                return Ok(Type::Array(Box::new(member)));
            }
            swc::TsType::TsConditionalType(c) => {
                let check_ty = self.translate_type(&c.check_type)?;
                let extends_ty = self.translate_type(&c.extends_type)?;
                let false_ty = self.translate_type(&c.false_type)?;
                let true_ty = self.translate_type(&c.true_type)?;

                let t = self.type_check(c.span, &check_ty, &extends_ty).is_ok();

                if t {
                    return Ok(true_ty);
                } else {
                    return Ok(false_ty);
                }
            }
            swc::TsType::TsFnOrConstructorType(func) => {
                let func = self.translate_func_type(func)?;

                return Ok(Type::Function(Box::new(func)));
            }
            swc::TsType::TsImportType(t) => {
                // TODO: import types
                return Err(Error::syntax_error(t.span, "import types not supported"));
            }
            swc::TsType::TsIndexedAccessType(i) => {
                if i.readonly {
                    // todo!()
                }

                let index_ty = self.translate_type(&i.index_type)?;
                let value_ty = self.translate_type(&i.obj_type)?;

                return Ok(Type::Map(Box::new(index_ty), Box::new(value_ty)));
            }
            swc::TsType::TsInferType(i) => {
                // TODO: infer types
                return Err(Error::syntax_error(i.span, "infer types not supported"));
            }
            swc::TsType::TsKeywordType(key) => {
                let ty = match key.kind {
                    swc::TsKeywordTypeKind::TsAnyKeyword => Type::Any,
                    swc::TsKeywordTypeKind::TsBigIntKeyword => Type::Bigint,
                    swc::TsKeywordTypeKind::TsBooleanKeyword => Type::Bool,
                    swc::TsKeywordTypeKind::TsIntrinsicKeyword => {
                        return Err(Error::syntax_error(
                            key.span,
                            "instrinsic types not supported",
                        ))
                    }
                    swc::TsKeywordTypeKind::TsNeverKeyword => {
                        //return Err(Error::syntax_error(key.span, "never type not supported"))
                        Type::Undefined
                    }
                    swc::TsKeywordTypeKind::TsNullKeyword => Type::Null,
                    swc::TsKeywordTypeKind::TsNumberKeyword => Type::Number,
                    swc::TsKeywordTypeKind::TsObjectKeyword => Type::AnyObject,
                    swc::TsKeywordTypeKind::TsStringKeyword => Type::String,
                    swc::TsKeywordTypeKind::TsSymbolKeyword => Type::Symbol,
                    swc::TsKeywordTypeKind::TsUndefinedKeyword => Type::Undefined,
                    swc::TsKeywordTypeKind::TsUnknownKeyword => Type::Any,
                    swc::TsKeywordTypeKind::TsVoidKeyword => {
                        Type::Union(Box::new([Type::Undefined, Type::Null]))
                    }
                };

                return Ok(ty);
            }
            swc::TsType::TsLitType(l) => {
                return Err(Error::syntax_error(l.span, "literal types not supported"))
            }
            swc::TsType::TsMappedType(m) => {
                return Err(Error::syntax_error(m.span, "mapped types not supported"))
            }
            swc::TsType::TsOptionalType(t) => {
                let ty = self.translate_type(&t.type_ann)?;

                return Ok(ty.union(Type::Undefined));
            }
            swc::TsType::TsParenthesizedType(p) => return self.translate_type(&p.type_ann),
            swc::TsType::TsRestType(t) => {
                return Err(Error::syntax_error(t.span, "rest type not supported"))
            }
            swc::TsType::TsThisType(_) => return Ok(self.this_ty.clone()),
            swc::TsType::TsTupleType(t) => {
                let mut tys = Vec::new();
                for i in &t.elem_types {
                    let ty = self.translate_type(&i.ty)?;
                    tys.push(ty);
                }

                return Ok(Type::Tuple(tys.into_boxed_slice()));
            }
            swc::TsType::TsTypeLit(l) => {
                return Err(Error::syntax_error(l.span, "type literal not supported"))
            }
            swc::TsType::TsTypeOperator(o) => {
                let ty = self.translate_type(&o.type_ann)?;

                match o.op {
                    swc::TsTypeOperatorOp::KeyOf => match ty {
                        Type::Map(k, _) => return Ok(*k),
                        _ => {
                            return Err(Error::syntax_error(
                                o.span,
                                "expected index accessing type",
                            ))
                        }
                    },
                    swc::TsTypeOperatorOp::ReadOnly => return Ok(ty),
                    swc::TsTypeOperatorOp::Unique => return Ok(ty),
                }
            }
            swc::TsType::TsTypePredicate(p) => match &p.param_name {
                swc::TsThisTypeOrIdent::Ident(_ident) => {
                    if let Some(ty) = &p.type_ann {
                        let _ty = self.translate_type(&ty.type_ann)?;
                        return Ok(Type::Bool);
                    } else {
                        return Err(Error::syntax_error(p.span, "missing type annotation"));
                    }
                }
                swc::TsThisTypeOrIdent::TsThisType(_t) => {
                    if let Some(ann) = &p.type_ann {
                        let _ty = self.translate_type(&ann.type_ann)?;

                        return Ok(Type::Bool);
                    } else {
                        return Err(Error::syntax_error(p.span, "missing type annotation"));
                    }
                }
            },
            swc::TsType::TsTypeQuery(q) => {
                // typeof operator
                let mut allow_generics = false;

                let ty = match &q.expr_name {
                    swc::TsTypeQueryExpr::TsEntityName(name) => match self.find_binding(name) {
                        Some(Binding::GenericClass(_)) | Some(Binding::Class(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, class is not a value",
                            ))
                        }
                        Some(Binding::Enum(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, enum is not a value",
                            ))
                        }
                        Some(Binding::GenericFunction(id)) => {
                            allow_generics = true;

                            if let Some(type_args) = &q.type_args {
                                let type_args = self.translate_type_args(&type_args)?;
                                let id = self.solve_generic_function(id, &type_args)?;

                                let func = self.context.functions.get(&id).unwrap();

                                Type::Function(Box::new(FuncType {
                                    this_ty: func.this_ty.clone(),
                                    params: func.params.iter().map(|p| p.ty.clone()).collect(),
                                    var_arg: false,
                                    return_ty: func.return_ty.clone(),
                                }))
                            } else {
                                return Err(Error::syntax_error(
                                    q.span,
                                    "cannot infer type, generic function has no concrete type",
                                ));
                            }
                        }
                        Some(Binding::Function(f)) => {
                            let func = self.context.functions.get(&f).unwrap();

                            Type::Function(Box::new(FuncType {
                                this_ty: func.this_ty.clone(),
                                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                                var_arg: false,
                                return_ty: func.return_ty.clone(),
                            }))
                        }
                        Some(Binding::GenericTypeAlias { .. }) | Some(Binding::Generic(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, generic is not a value",
                            ))
                        }
                        Some(Binding::GenericInterface(_)) | Some(Binding::Interface(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, interface is not a value",
                            ))
                        }
                        Some(Binding::NameSpace(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, namespace is not a value",
                            ))
                        }
                        Some(Binding::TypeAlias(_)) => {
                            return Err(Error::syntax_error(
                                q.span,
                                "cannot infer type, type alias is not a value",
                            ))
                        }
                        Some(Binding::Using { ty, .. }) => ty.clone(),
                        Some(Binding::Var { ty, .. }) => ty.clone(),
                        None => {
                            return Err(Error::syntax_error(name.span(), "undefined identifier"))
                        }
                    },
                    swc::TsTypeQueryExpr::Import(_) => {
                        todo!("import type")
                    }
                };

                // should not have type arguments
                if !allow_generics {
                    if let Some(args) = &q.type_args {
                        if args.params.len() != 0 {
                            return Err(Error::syntax_error(
                                args.span,
                                "expected zero type arguments",
                            ));
                        }
                    }
                }

                return Ok(ty);
            }
            swc::TsType::TsTypeRef(r) => {
                let binding = match self.find_binding(&r.type_name) {
                    Some(b) => b,
                    None => {
                        return Err(Error::syntax_error(
                            r.type_name.span(),
                            "undefined identifier",
                        ))
                    }
                };

                let mut generics_allowed = false;

                let ty = match binding {
                    Binding::Class(c) => Type::Object(c),
                    Binding::Enum(e) => Type::Enum(e),
                    Binding::Interface(i) => Type::Interface(i),
                    Binding::TypeAlias(id) => {
                        if let Some(ty) = self.context.alias.get(&id) {
                            ty.clone()
                        } else {
                            Type::Alias(id)
                        }
                    }
                    Binding::GenericFunction(_) | Binding::Function(_) => {
                        return Err(Error::syntax_error(
                            r.type_name.span(),
                            "expected type, found function",
                        ))
                    }
                    Binding::Generic(_) => todo!("generics"),
                    Binding::GenericClass(id) => {
                        generics_allowed = true;

                        if let Some(type_args) = &r.type_params {
                            let type_args = self.translate_type_args(&type_args)?;
                            let id = self.solve_generic_class(id, &type_args)?;
                            Type::Object(id)
                        } else {
                            return Err(Error::syntax_error(r.span, "missing type arguments"));
                        }
                    }

                    Binding::GenericInterface(id) => {
                        generics_allowed = true;

                        if let Some(type_args) = &r.type_params {
                            let type_args = self.translate_type_args(&type_args)?;
                            let id = self.solve_generic_interface(id, &type_args)?;
                            Type::Interface(id)
                        } else {
                            return Err(Error::syntax_error(r.span, "missing type arguments"));
                        }
                    }
                    Binding::GenericTypeAlias(id) => {
                        generics_allowed = true;

                        if let Some(type_args) = &r.type_params {
                            let type_args = self.translate_type_args(&type_args)?;
                            let id = self.solve_generic_alias(id, &type_args)?;

                            if let Some(ty) = self.context.alias.get(&id) {
                                ty.clone()
                            } else {
                                Type::Alias(id)
                            }
                        } else {
                            return Err(Error::syntax_error(r.span, "missing type arguments"));
                        }
                    }
                    Binding::NameSpace(_) => {
                        return Err(Error::syntax_error(
                            r.type_name.span(),
                            "expected type, found name space",
                        ))
                    }
                    Binding::Using { .. } | Binding::Var { .. } => {
                        return Err(Error::syntax_error(
                            r.type_name.span(),
                            "expected type, found variable",
                        ))
                    }
                };

                if !generics_allowed {
                    if let Some(args) = &r.type_params {
                        if args.params.len() != 0 {
                            return Err(Error::syntax_error(
                                args.span,
                                "expected 0 type arguments",
                            ));
                        }
                    }
                }

                return Ok(ty);
            }
            swc::TsType::TsUnionOrIntersectionType(u) => match u {
                swc::TsUnionOrIntersectionType::TsIntersectionType(i) => {
                    return self.translate_intersection_type(i)
                }
                swc::TsUnionOrIntersectionType::TsUnionType(u) => {
                    let mut tys = Vec::new();

                    for ty in &u.types {
                        let ty = self.translate_type(&ty)?;

                        if !tys.contains(&ty) {
                            tys.push(ty);
                        }
                    }

                    tys.sort();

                    return Ok(Type::Union(tys.into_boxed_slice()));
                }
            },
        }
    }

    pub fn translate_func_type(&mut self, func: &swc::TsFnOrConstructorType) -> Result<FuncType> {
        match func {
            swc::TsFnOrConstructorType::TsConstructorType(cons) => {
                return Err(Error::syntax_error(
                    cons.span,
                    "constructor type not allowed",
                ))
            }
            swc::TsFnOrConstructorType::TsFnType(func) => {
                if func.type_params.is_some() {
                    return Err(Error::syntax_error(
                        func.span,
                        "fucntion type cannot be generic",
                    ));
                }

                // this type default to any
                let mut this_ty = Type::Any;
                let mut params = Vec::new();

                // translate params
                for (i, p) in func.params.iter().enumerate() {
                    if let swc::TsFnParam::Ident(id) = p {
                        if id.type_ann.is_none() {
                            return Err(Error::syntax_error(id.span, "missing type annotation"));
                        }
                        let mut ty =
                            self.translate_type(&id.type_ann.as_ref().unwrap().type_ann)?;

                        // optional type
                        if id.optional {
                            ty = ty.union(Type::Undefined);
                        }

                        // explicit this type
                        if id.sym.as_ref() == "this" && i == 0 {
                            this_ty = ty;
                            continue;
                        }

                        params.push(ty);
                    } else {
                        // function type should not be destructive
                        return Err(Error::syntax_error(
                            p.span(),
                            "destructive params not allowed",
                        ));
                    }
                }

                // return type
                let return_ty = self.translate_type(&func.type_ann.type_ann)?;

                return Ok(FuncType {
                    this_ty,
                    params: params,
                    var_arg: false,
                    return_ty,
                });
            }
        };
    }

    pub fn translate_intersection_type(
        &mut self,
        intersec: &swc::TsIntersectionType,
    ) -> Result<Type> {
        let mut iface = InterfaceType {
            name: self.anonymous_name(),
            extends: Vec::new(),
            implements: Vec::new(),
            properties: Default::default(),
            methods: Default::default(),
        };

        for ty in &intersec.types {
            let ty = self.translate_type(&ty)?;

            match ty{
                Type::Interface(id) => {
                    if !iface.implements.contains(&id){
                        iface.implements.push(id);
                    }
                }
                Type::Object(id) => {
                    // only push if not already exist
                    if !iface.extends.contains(&id){
                        iface.extends.push(id);
                    }
                }
                _ => {
                    return Err(Error::syntax_error(intersec.span, "An intersection cannot extend a primitive type; only interfaces and classes can intersect"))
                }
            };
        }

        let id = InterfaceId::new();

        self.context.interfaces.insert(id, iface);

        return Ok(Type::Interface(id));
    }

    fn find_binding(&mut self, entity_name: &swc::TsEntityName) -> Option<Binding> {
        match entity_name {
            swc::TsEntityName::Ident(id) => return self.context.find(&id.sym).map(|b| b.clone()),
            swc::TsEntityName::TsQualifiedName(q) => {
                let left = self.find_binding(&q.left)?;

                match left {
                    Binding::NameSpace(namespace) => {
                        return self.find_binding_from_module(
                            namespace,
                            &PropName::Ident(q.right.sym.to_string()),
                        );
                    }
                    _ => return None,
                }
            }
        }
    }

    pub fn translate_type_args(
        &mut self,
        args: &swc::TsTypeParamInstantiation,
    ) -> Result<Vec<Type>> {
        let mut v = Vec::with_capacity(args.params.len());

        for ty in &args.params {
            v.push(self.translate_type(&ty)?);
        }

        return Ok(v);
    }

    pub fn solve_generic_function(
        &mut self,
        _id: FunctionId,
        _type_args: &[Type],
    ) -> Result<FunctionId> {
        todo!("generic function")
    }

    pub fn solve_generic_class(&mut self, _id: ClassId, _type_args: &[Type]) -> Result<ClassId> {
        todo!("generic class")
    }

    pub fn solve_generic_interface(
        &mut self,
        _id: InterfaceId,
        _type_args: &[Type],
    ) -> Result<InterfaceId> {
        todo!("generic class")
    }

    pub fn solve_generic_alias(&mut self, _id: AliasId, _type_args: &[Type]) -> Result<AliasId> {
        todo!("generic class")
    }

    // returns the iterator type, iterator result type and the value type
    pub fn type_is_iterable(&self, span: Span, iterable_ty: &Type) -> Result<(Type, Type, Type)> {
        let iterator_result_ty: Type;
        let value_ty: Type;

        let iterator_func_ty = match self.type_has_property(
            &iterable_ty,
            &PropName::Symbol(crate::Symbol::Iterator),
            true,
        ) {
            Some(ty) => ty,
            None => {
                return Err(Error::syntax_error(
                    span,
                    format!("type '' is not iterable, missing property [Symbol.iterator]"),
                ))
            }
        };

        let iterator_ty = match iterator_func_ty {
            Type::Function(func) => {
                // param must be empty
                if !func.params.is_empty() {
                    return Err(Error::syntax_error(span, format!("type '' is not iterable, property [Symbol.iterator] is expected to have 0 arguments")));
                }
                // check this type matches iterable
                match self.type_check(span, &iterable_ty, &func.this_ty){
                    Ok(_) => {},
                    Err(_) => {
                        return Err(Error::syntax_error(span, format!("type '' is not iterable, property [Symbol.iterator] has mismatched 'this' argument: type '' is not assignable to ''")))
                    }
                };
                // the return type is the iterator type
                func.return_ty
            }
            _ => {
                return Err(Error::syntax_error(
                    span,
                    format!("type '' is not iterable, property [Symbol.iterator] is not callable"),
                ))
            }
        };

        // check iterator have next() method
        match self.type_has_property(&iterator_ty, &PropName::Ident("next".to_string()), true) {
            Some(next_func_ty) => {
                // check next is a function
                iterator_result_ty = match next_func_ty{
                    Type::Function(func) => {
                        // param must be empty
                        if !func.params.is_empty(){
                            return Err(Error::syntax_error(span, format!("type '' is not iterable, property [Symbol.iterator]().next is expected to have 0 arguments")))
                        }
                        // 'this' type must be equal to iterator
                        match self.type_check(span, &iterator_ty, &func.this_ty){
                            Ok(_) => {},
                            Err(_) => {
                                return Err(Error::syntax_error(span, format!("type '' is not iterable, property [Symbol.iterator]().next has mismatched 'this' argument: type '' is not assignable to ''")))
                            }
                        };
                        // the return type is iterator result
                        func.return_ty
                    }
                    // next is not a function
                    _ => return Err(Error::syntax_error(span, format!("type '' is not iterable, property [Symbol.iterator]().next is not callable")))
                };

                if self
                    .type_has_property(
                        &iterator_result_ty,
                        &PropName::Ident("done".to_string()),
                        false,
                    )
                    .is_none()
                {
                    return Err(Error::syntax_error(span, format!("type '' is not iterable, missing property [Symbol.iterator]().next().done")));
                }
                if let Some(value) = self.type_has_property(
                    &iterator_result_ty,
                    &PropName::Ident("value".to_string()),
                    false,
                ) {
                    value_ty = value;
                } else {
                    return Err(Error::syntax_error(span, format!("type '' is not iterable, missing property [Symbol.iterator]().next().value")));
                }
            }
            // no property next
            None => {
                return Err(Error::syntax_error(
                    span,
                    format!("type '' is not iterable, missing property [Symbol.iterator]().next"),
                ))
            }
        };

        return Ok((iterator_ty, iterator_result_ty, value_ty));
    }

    pub fn type_check(&self, span: Span, ty: &Type, fulfills: &Type) -> Result<()> {
        // fast return
        if ty == fulfills {
            return Ok(());
        }

        match fulfills {
            // every type can be converted to any and bool
            Type::Bool | Type::Any => return Ok(()),
            // alias is only used when hoisting
            Type::Alias(_) => unreachable!("unresolved alias"),
            Type::AnyObject => {
                if ty.is_object() {
                    return Ok(());
                }
            }
            // these types must be strictly obayed
            Type::Array(_)
            | Type::Bigint
            | Type::Enum(_)
            | Type::Function(_)
            | Type::Generic(_)
            | Type::Map(_, _)
            | Type::Null
            | Type::Promise(_)
            | Type::Regex
            | Type::String
            | Type::Symbol
            | Type::Tuple(_)
            | Type::Undefined => {}
            // number and int are compatable
            Type::Number | Type::Int => {
                if ty == &Type::Number || ty == &Type::Int {
                    return Ok(());
                }
            }
            // a union requirement just have to fulill a single alternative
            Type::Union(u) => {
                // if ty is a union, all elements must respect fulfills
                if let Type::Union(u) = ty {
                    for t in u.iter() {
                        self.type_check(span, t, fulfills)?;
                    }
                } else {
                    // if ty fulfills any one of union, it is true
                    if u.iter().any(|t| self.type_check(span, ty, t).is_ok()) {
                        return Ok(());
                    }
                }
            }
            Type::Interface(iface) => {
                let iface = self
                    .context
                    .interfaces
                    .get(iface)
                    .expect("invalid interface");

                for im in &iface.implements {
                    self.type_check(span, ty, &Type::Interface(*im))?;
                }

                for ex in &iface.extends {
                    self.type_check(span, ty, &Type::Object(*ex))?;
                }

                for (name, attr) in &iface.properties {
                    if let Some(attr_ty) = self.type_has_property(ty, name, false) {
                        self.type_check(span, &attr_ty, &attr.ty)?;
                    } else {
                        if attr.optional {
                            continue;
                        }
                        return Err(Error::syntax_error(
                            span,
                            format!(
                                "Property '{}' is missing in type '' but required in type ''",
                                name
                            ),
                        ));
                    }
                }

                for (name, method) in &iface.methods {
                    if let Some(attr) = self.type_has_property(ty, name, true) {
                        if let Type::Function(func) = &attr {
                            // this of function must be equal to ty
                            if &func.this_ty != ty {
                                return Err(Error::syntax_error(
                                    span,
                                    format!(
                                        "Method '{}' is missing in type '' but required in type ''",
                                        name
                                    ),
                                ));
                            }
                            if &func.params != &method.params {
                                return Err(Error::syntax_error(
                                    span,
                                    format!("Method '{}' have incompatable arguments", name),
                                ));
                            }
                            if &func.return_ty != &method.return_ty {
                                return Err(Error::syntax_error(
                                    span,
                                    format!("Method '{}' have incompatable return types", name),
                                ));
                            }
                        }
                    } else {
                        if method.optional {
                            continue;
                        }
                        return Err(Error::syntax_error(
                            span,
                            format!(
                                "Method '{}' is missing in type '' but required in type ''",
                                name
                            ),
                        ));
                    }
                }
                return Ok(());
            }
            Type::Object(class_id) => {
                if let Type::Object(obj_ty_id) = ty {
                    // fast return
                    if obj_ty_id == class_id {
                        return Ok(());
                    }

                    let obj_class = self.context.classes.get(obj_ty_id).expect("invalid class");

                    // check the super type
                    if let Some(super_class_id) = obj_class.extends {
                        // super type must fulfil requirement
                        return self.type_check(span, &Type::Object(super_class_id), fulfills);
                    }
                }
            }
            Type::Iterator(iter_elem) => {
                if let Some(next_ty) =
                    self.type_has_property(ty, &PropName::Ident("next".to_owned()), true)
                {
                    if let Type::Function(func) = &next_ty {
                        if iter_elem.as_ref() == &func.return_ty {
                            return Ok(());
                        }
                    }
                }
            }
        }
        return Err(Error::syntax_error(
            span,
            format!("type '' is not assignable to type ''"),
        ));
    }

    pub fn type_has_property(&self, ty: &Type, prop: &PropName, method: bool) -> Option<Type> {
        if let PropName::Ident(ident) = prop {
            if ident == "toString" {
                return Some(Type::Function(Box::new(FuncType {
                    this_ty: Type::Any,
                    params: Vec::new(),
                    var_arg: false,
                    return_ty: Type::String,
                })));
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
            Type::Object(class) => {
                let class = self.context.classes.get(class).expect("invalid class");
                if method {
                    if let Some((_id, func_ty)) = class.methods.get(prop) {
                        return Some(Type::Function(Box::new(func_ty.clone())));
                    }
                }
                if let Some(attr) = class.properties.get(prop) {
                    return Some(attr.ty.clone());
                }
                if let Some((_id, func_ty)) = class.methods.get(prop) {
                    return Some(Type::Function(Box::new(func_ty.clone())));
                }
                return None;
            }
            Type::Interface(id) => {
                let iface = self.context.interfaces.get(id).expect("invalid interface");
                if method {
                    if let Some(m) = iface.methods.get(prop) {
                        return Some(Type::Function(Box::new(FuncType {
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
                    return Some(Type::Function(Box::new(FuncType {
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
                    if *index >= elems.len() as i32{
                        return None
                    }
                    if *index < 0{
                        return None
                    }
                    return Some(elems[*index as usize].clone())
                },
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
                            "at" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Int.union(Type::Undefined)],
                                var_arg: false,
                                return_ty: Type::Undefined.union(elem.as_ref().clone()),
                            }))),
                            "concat" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Array(elem.clone())],
                                var_arg: true,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "copyWithin" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Int,
                                    Type::Int,
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "entries" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(Box::new(Type::Tuple(Box::new([
                                    Type::Int,
                                    elem.as_ref().clone(),
                                ])))),
                            }))),
                            "every" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Box::new(FuncType {
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
                            "fill" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "filter" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "find" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "findIndex" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "findLast" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "findLastIndex" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "flatMap" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "forEach" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "includes" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Bool,
                            }))),
                            "indexOf" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "lastIndexOf" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    elem.as_ref().clone(),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Int,
                            }))),
                            "join" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::String.union(Type::Undefined)],
                                var_arg: false,
                                return_ty: Type::String,
                            }))),
                            "keys" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(Box::new(Type::Int)),
                            }))),
                            "map" => todo!(),
                            "pop" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "push" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![elem.as_ref().clone()],
                                var_arg: true,
                                return_ty: Type::Int,
                            }))),
                            "reduce" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Box::new(FuncType {
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
                            "reduceRight" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Function(Box::new(FuncType {
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
                            "reverse" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Undefined,
                            }))),
                            "shift" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: elem.as_ref().clone().union(Type::Undefined),
                            }))),
                            "slice" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    Type::Int.union(Type::Undefined),
                                    Type::Int.union(Type::Undefined),
                                ],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "some" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![
                                    // callback
                                    Type::Function(Box::new(FuncType {
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
                            "sort" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Function(Box::new(FuncType {
                                    this_ty: Type::Any,
                                    params: vec![elem.as_ref().clone(), elem.as_ref().clone()],
                                    var_arg: false,
                                    return_ty: Type::Int,
                                }))],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "splice" => Some(Type::Function(Box::new(FuncType {
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
                            "toReverse" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "toSorted" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Function(Box::new(FuncType {
                                    this_ty: Type::Any,
                                    params: vec![elem.as_ref().clone(), elem.as_ref().clone()],
                                    var_arg: false,
                                    return_ty: Type::Int,
                                }))],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            "toSplice" => Some(Type::Function(Box::new(FuncType {
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
                            "unshift" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![elem.as_ref().clone()],
                                var_arg: true,
                                return_ty: Type::Int,
                            }))),
                            "values" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![],
                                var_arg: false,
                                return_ty: Type::Iterator(elem.clone()),
                            }))),
                            "with" => Some(Type::Function(Box::new(FuncType {
                                this_ty: Type::Array(elem.clone()),
                                params: vec![Type::Int, elem.as_ref().clone()],
                                var_arg: false,
                                return_ty: Type::Array(elem.clone()),
                            }))),
                            _ => None,
                        }
                    }
                    PropName::Symbol(sym) => match sym {
                        Symbol::Iterator => Some(Type::Iterator(elem.clone())),
                        Symbol::Unscopables => todo!(),
                        _ => None,
                    },
                }
            }
            _ => todo!(),
        }
    }
}
