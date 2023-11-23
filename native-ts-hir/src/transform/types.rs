use native_js_common::error::Error;
use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::ast::{ClassType, EnumType, EnumVariantDesc, FuncType, InterfaceType, Type};
use crate::common::{AliasId, ClassId, FunctionId, InterfaceId};
use crate::PropName;

type Result<T> = std::result::Result<T, Error<Span>>;

use super::{context::Binding, Transformer};

impl Transformer {
    /// this function only translates type definitions and does not translate any initialiser or methods
    pub fn translate_class_ty(&mut self, class: &swc::Class) -> Result<ClassType> {
        todo!()
    }

    pub fn translate_interface(&mut self, iface: &swc::TsInterfaceDecl) -> Result<InterfaceType> {
        todo!()
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

    pub fn translate_type(&mut self, ty: &swc::TsType) -> Result<Type> {
        match ty {
            swc::TsType::TsArrayType(array) => {
                let member = self.translate_type(&array.elem_type)?;
                return Ok(Type::Array(Box::new(member)));
            }
            swc::TsType::TsConditionalType(c) => {
                let _check_ty = self.translate_type(&c.check_type)?;
                let _extends_ty = self.translate_type(&c.extends_type)?;
                let _false_ty = self.translate_type(&c.false_type)?;
                let _true_ty = self.translate_type(&c.true_type)?;

                return Err(Error::syntax_error(
                    c.span,
                    "conditional type not allowed. All types should be known at compile time",
                ));
            }
            swc::TsType::TsFnOrConstructorType(func) => {
                let func = self.translate_func_type(func)?;

                return Ok(Type::Function(Box::new(func)));
            }
            swc::TsType::TsImportType(_t) => {
                todo!("import type")
            }
            swc::TsType::TsIndexedAccessType(i) => {
                if i.readonly {
                    // todo!()
                }

                let index_ty = self.translate_type(&i.index_type)?;
                let value_ty = self.translate_type(&i.obj_type)?;

                return Ok(Type::Map(Box::new(index_ty), Box::new(value_ty)));
            }
            swc::TsType::TsInferType(_i) => {
                todo!("infer type")
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
                    swc::TsTypeOperatorOp::KeyOf => {
                        todo!("keyof type")
                    }
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

                            if let Some(args) = &q.type_args {
                                let id = self.solve_generic_function(id, &args)?;

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
                            let id = self.solve_generic_class(id, type_args)?;
                            Type::Object(id)
                        } else {
                            return Err(Error::syntax_error(r.span, "missing type arguments"));
                        }
                    }

                    Binding::GenericInterface(id) => {
                        generics_allowed = true;

                        if let Some(type_args) = &r.type_params {
                            let id = self.solve_generic_interface(id, type_args)?;
                            Type::Interface(id)
                        } else {
                            return Err(Error::syntax_error(r.span, "missing type arguments"));
                        }
                    }
                    Binding::GenericTypeAlias(id) => {
                        generics_allowed = true;

                        if let Some(type_args) = &r.type_params {
                            let id = self.solve_generic_alias(id, type_args)?;

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
                    return Err(Error::syntax_error(func.span, ""));
                }

                let mut this_ty = Type::Any;
                let mut params = Vec::new();

                for (i, p) in func.params.iter().enumerate() {
                    match p {
                        swc::TsFnParam::Ident(id) => {
                            if id.type_ann.is_none() {
                                return Err(Error::syntax_error(id.span, ""));
                            }
                            let mut ty =
                                self.translate_type(&id.type_ann.as_ref().unwrap().type_ann)?;

                            if id.optional {
                                ty = ty.union(Type::Undefined);
                            }

                            if id.sym.as_ref() == "this" {
                                if i == 0 {
                                    this_ty = ty;
                                    continue;
                                } else {
                                    return Err(Error::syntax_error(
                                        id.span,
                                        "'this' keyword is only allowed at the first param",
                                    ));
                                }
                            }

                            params.push(ty);
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                p.span(),
                                "destructive params not allowed",
                            ))
                        }
                    }
                }

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

    pub fn solve_generic_function(
        &mut self,
        id: FunctionId,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<FunctionId> {
        todo!("generic function")
    }

    pub fn solve_generic_class(
        &mut self,
        id: ClassId,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<ClassId> {
        todo!("generic class")
    }

    pub fn solve_generic_interface(
        &mut self,
        id: InterfaceId,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<InterfaceId> {
        todo!("generic class")
    }

    pub fn solve_generic_alias(
        &mut self,
        id: AliasId,
        type_args: &swc::TsTypeParamInstantiation,
    ) -> Result<AliasId> {
        todo!("generic class")
    }

    pub fn type_check(&mut self, span: Span, ty: &Type, fulfills: &Type) -> Result<()> {
        todo!()
    }

    pub fn type_has_property(&mut self, span:Span, ty: &Type, prop: &PropName) -> Option<Type>{

        match ty{
            _ => {}
        }
        return None
    }
}
