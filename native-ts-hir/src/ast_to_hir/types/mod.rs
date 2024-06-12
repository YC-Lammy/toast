use std::collections::HashMap;

use native_ts_parser::swc_core::common::{Span, Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;
use num_traits::ToPrimitive;

use crate::common::{AliasId, ClassId, FunctionId, InterfaceId, ModuleId};
use crate::error::Error;
use crate::hir::{
    EnumType, EnumVariantDesc, Expr, FuncType, InterfaceMethod, InterfacePropertyDesc,
    InterfaceType, MaybeGenericType, ModuleTypeExport, ModuleValueExport, PropNameOrExpr,
    PropertyDesc, Type,
};
use crate::PropName;

use super::Transformer;
use super::{ClassBinding, TypeBinding, ValueBinding};

pub mod class;
type Result<T> = std::result::Result<T, Error>;

impl Transformer {
    pub fn generalise_type(&self, expr: &mut Expr, ty: &Type) -> Option<Type> {
        match ty {
            Type::Int | Type::LiteralInt(_) => {
                self.cast(expr, &ty, &Type::Number);
                Some(Type::Number)
            }
            Type::LiteralNumber(_) => Some(Type::Number),
            Type::LiteralString(_) => Some(Type::String),
            Type::LiteralBigint(_) => Some(Type::Bigint),
            Type::LiteralBool(_) => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn cast(&self, expr: &mut Expr, expr_ty: &Type, ty: &Type) {
        if expr_ty == ty {
            return;
        }
        if let Type::LiteralInt(_) = expr_ty {
            if ty == &Type::Int {
                return;
            }
        }
        if let Type::LiteralNumber(_) = expr_ty {
            if ty == &Type::Number {
                return;
            }
        }
        if let Type::LiteralBool(_) = expr_ty {
            if ty == &Type::Bool {
                return;
            }
        }
        if let Type::LiteralBigint(_) = expr_ty {
            if ty == &Type::Bigint {
                return;
            }
        }
        if let Type::LiteralString(_) = expr_ty {
            if ty == &Type::String {
                return;
            }
        }

        fn inner_cast(expr: &mut Expr, ty: &Type) {
            unsafe {
                let e = Expr::Cast {
                    span: DUMMY_SP,
                    value: Box::new(core::ptr::read(expr)),
                    ty: ty.clone(),
                };
                core::ptr::write(expr, e);
            }
        }

        match &expr {
            Expr::Int(i) => match &ty {
                Type::Number => *expr = Expr::Number(*i as f64),
                Type::Bool => *expr = Expr::Bool(*i != 0),
                Type::String => *expr = Expr::String(i.to_string()),
                _ => inner_cast(expr, ty),
            },
            Expr::Bigint(i) => match &ty {
                Type::Bool => *expr = Expr::Bool(*i != 0),
                Type::String => *expr = Expr::String(i.to_string()),
                _ => inner_cast(expr, ty),
            },
            Expr::Bool(b) => match &ty {
                Type::Number => *expr = Expr::Number(if *b { 1.0 } else { 0.0 }),
                Type::Bigint => *expr = Expr::Bigint(if *b { 1 } else { 0 }),
                Type::Int => *expr = Expr::Int(if *b { 1 } else { 0 }),
                Type::String => *expr = Expr::String(b.to_string()),
                _ => inner_cast(expr, ty),
            },
            Expr::Number(n) => match &ty {
                Type::Int => *expr = Expr::Int(*n as i32),
                Type::Bigint => *expr = Expr::Bigint(*n as i128),
                Type::Bool => *expr = Expr::Bool(!n.is_nan() && *n != 0.0),
                Type::String => *expr = Expr::String(n.to_string()),
                _ => inner_cast(expr, ty),
            },
            Expr::String(s) => match &ty {
                Type::Bool => *expr = Expr::Bool(!s.is_empty()),
                _ => inner_cast(expr, ty),
            },
            Expr::Undefined => match &ty {
                Type::Int => *expr = Expr::Int(0),
                Type::Number => *expr = Expr::Number(0.0),
                Type::Bigint => *expr = Expr::Bigint(0),
                Type::Bool => *expr = Expr::Bool(false),
                Type::String => *expr = Expr::String("undefined".to_string()),
                _ => inner_cast(expr, ty),
            },
            Expr::Null => match &ty {
                Type::Int => *expr = Expr::Int(0),
                Type::Number => *expr = Expr::Number(0.0),
                Type::Bigint => *expr = Expr::Bigint(0),
                Type::Bool => *expr = Expr::Bool(false),
                Type::String => *expr = Expr::String("null".to_string()),
                _ => inner_cast(expr, ty),
            },
            _ => inner_cast(expr, ty),
        }
    }

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
            return_ty = Type::Iterator(return_ty.into());
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
                                "property name of interface must be literal",
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
        let ty = self.find_expr_type_binding(&expr)?;

        match ty {
            MaybeGenericType::Type(ty) => {
                if !type_args.is_empty() {
                    return Err(Error::syntax_error(
                        expr.span(),
                        "expecting 0 type arguments",
                    ));
                }
                return Ok(ty);
            }
            _ => todo!("generics"),
        }
    }

    fn find_expr_namespace(&mut self, expr: &swc::Expr) -> Result<ModuleId> {
        match expr {
            // find from inner expression
            swc::Expr::Paren(p) => return self.find_expr_namespace(&p.expr),
            // find namespace from namespace
            swc::Expr::Member(m) => {
                let name = match &m.prop {
                    swc::MemberProp::Computed(p) => self.translate_computed_prop_name(&p.expr)?,
                    swc::MemberProp::Ident(id) => {
                        PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
                    }
                    swc::MemberProp::PrivateName(p) => {
                        PropNameOrExpr::PropName(PropName::Private(p.id.sym.to_string()))
                    }
                };

                let name = match name {
                    PropNameOrExpr::PropName(p) => p,
                    PropNameOrExpr::Expr(_, _) => {
                        return Err(Error::syntax_error(
                            m.prop.span(),
                            "computed property name not allowed",
                        ))
                    }
                };

                let namespace = self.find_expr_namespace(&m.obj)?;

                let (_, _, namespace) = self.module_export(namespace, &name);

                match namespace {
                    Some(namespace) => Ok(namespace),
                    None => {
                        return Err(Error::syntax_error(
                            m.span,
                            format!("'{}' is not a namespace", name),
                        ))
                    }
                }
            }
            // find namespace binding
            swc::Expr::Ident(id) => match self.context.find_namespace_binding(&id.sym) {
                Some(namespace) => Ok(namespace),
                None => {
                    return Err(Error::syntax_error(
                        id.span,
                        format!("'{}' is not a namespace", id.sym),
                    ))
                }
            },
            _ => return Err(Error::syntax_error(expr.span(), "invalid expression")),
        }
    }

    fn find_expr_type_binding(&mut self, expr: &swc::Expr) -> Result<MaybeGenericType> {
        match expr {
            swc::Expr::Paren(p) => return self.find_expr_type_binding(&p.expr),
            swc::Expr::Member(m) => {
                let name = match &m.prop {
                    swc::MemberProp::Computed(p) => self.translate_computed_prop_name(&p.expr)?,
                    swc::MemberProp::Ident(id) => {
                        PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
                    }
                    swc::MemberProp::PrivateName(p) => {
                        PropNameOrExpr::PropName(PropName::Private(p.id.sym.to_string()))
                    }
                };

                let name = match name {
                    PropNameOrExpr::PropName(p) => p,
                    PropNameOrExpr::Expr(_, _) => {
                        return Err(Error::syntax_error(
                            m.prop.span(),
                            "computed property name not allowed",
                        ))
                    }
                };

                let namespace = self.find_expr_namespace(&m.obj)?;

                match self.find_type_from_namespace(namespace, &name) {
                    Some(ty) => return Ok(ty),
                    None => Err(Error::syntax_error(m.span, "value is not a type")),
                }
            }
            swc::Expr::Ident(id) => {
                // find binding from the current context
                match self.context.find_class_binding(&id.sym) {
                    Some(ClassBinding::Class(id)) => {
                        // binding is an object type
                        return Ok(MaybeGenericType::Type(Type::Object(*id)));
                    }
                    Some(ClassBinding::Generic(id)) => {
                        // binding is generic
                        return Ok(MaybeGenericType::GenericClass(*id));
                    }
                    None => {}
                };

                match self.context.find_type_binding(&id.sym) {
                    Some(TypeBinding::Enum(e)) => {
                        // binding is enum
                        return Ok(MaybeGenericType::Type(Type::Enum(*e)));
                    }
                    Some(TypeBinding::Interface(id)) => {
                        // binding is interface
                        return Ok(MaybeGenericType::Type(Type::Interface(*id)));
                    }
                    Some(TypeBinding::GenericInterface(id)) => {
                        // binding is generic interface
                        return Ok(MaybeGenericType::GenericInterface(*id));
                    }
                    Some(TypeBinding::TypeAlias(id)) => {
                        // binding is type alias
                        return Ok(MaybeGenericType::Type(Type::Alias(*id)));
                    }
                    Some(TypeBinding::GenericTypeAlias(id)) => {
                        // binding is generic type alias
                        return Ok(MaybeGenericType::GenericTypeAlias(*id));
                    }
                    Some(TypeBinding::Generic(id)) => {
                        // binding is generic
                        return Ok(MaybeGenericType::Generic(*id));
                    }
                    None => {}
                }

                return Err(Error::syntax_error(
                    id.span,
                    format!("undefined identifier '{}'", id.sym),
                ));
            }
            _ => Err(Error::syntax_error(expr.span(), "invalid type exprssion")),
        }
    }

    pub fn translate_type(&mut self, ty: &swc::TsType) -> Result<Type> {
        match ty {
            swc::TsType::TsArrayType(array) => {
                let member = self.translate_type(&array.elem_type)?;
                return Ok(Type::Array(member.into()));
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

                return Ok(Type::Function(func.into()));
            }
            swc::TsType::TsImportType(t) => {
                // TODO: import types
                return Err(Error::syntax_error(t.span, "import types not supported"));
            }
            swc::TsType::TsIndexedAccessType(i) => {
                // the index access type
                let index_ty = self.translate_type(&i.index_type)?;
                // the object type being accessed
                let value_ty = self.translate_type(&i.obj_type)?;

                match &value_ty {
                    // array types can use number type as index access type
                    Type::Array(elem) => match &index_ty {
                        // number types
                        Type::Number | Type::LiteralNumber(_) | Type::Int | Type::LiteralInt(_) => {
                            // return a clone of element type
                            return Ok(elem.as_ref().clone());
                        }
                        // otherwise fall through
                        _ => {}
                    },
                    // tuple types can use number type as index access type
                    Type::Tuple(elem) => match &index_ty {
                        // number type with unknown index
                        Type::Number | Type::Int => {
                            // return a union with possible element types
                            return Ok(Type::Union(elem.clone()));
                        }
                        // otherwise fall through
                        _ => {}
                    },
                    // fall through
                    _ => {}
                }

                // create a property name out of literal types
                let key = match index_ty {
                    // integer
                    Type::LiteralInt(i) => PropName::Int(i),
                    // number
                    Type::LiteralNumber(i) => PropName::Int(i.0 as i32),
                    // string
                    Type::LiteralString(s) => PropName::String(s.to_string()),
                    // not a supported index access type
                    _ => {
                        return Err(Error::syntax_error(
                            i.index_type.span(),
                            format!("type '' cannot be used as index access type"),
                        ))
                    }
                };

                // find the property type
                if let Some(ty) = self.type_has_property(&value_ty, &key, false) {
                    // return property type
                    return Ok(ty);
                } else {
                    // object type does not have the property
                    return Err(Error::syntax_error(
                        i.index_type.span(),
                        format!("type '' has no property '{}'", key),
                    ));
                }
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
                        Type::Union([Type::Undefined, Type::Null].into())
                    }
                };

                return Ok(ty);
            }
            swc::TsType::TsLitType(l) => return self.translate_literal_types(l),
            swc::TsType::TsMappedType(m) => {
                return Err(Error::compiler_error(m.span, "mapped types not supported"))
            }
            swc::TsType::TsOptionalType(t) => {
                let ty = self.translate_type(&t.type_ann)?;

                return Ok(ty.union(Type::Undefined));
            }
            swc::TsType::TsParenthesizedType(p) => return self.translate_type(&p.type_ann),
            swc::TsType::TsRestType(t) => {
                return Err(Error::compiler_error(t.span, "rest type not supported"))
            }
            swc::TsType::TsThisType(_) => return Ok(self.this_ty.clone()),
            swc::TsType::TsTupleType(t) => {
                let mut tys = Vec::new();
                for i in &t.elem_types {
                    let ty = self.translate_type(&i.ty)?;
                    tys.push(ty);
                }

                return Ok(Type::Tuple(tys.into()));
            }
            swc::TsType::TsTypeLit(l) => return self.translate_literal_object_type(l),
            swc::TsType::TsTypeOperator(o) => {
                // translate the type
                let ty = self.translate_type(&o.type_ann)?;

                match o.op {
                    swc::TsTypeOperatorOp::KeyOf => {
                        // vector for union type
                        let mut elem = Vec::new();

                        // loop through properties
                        for p in self.get_properties(&ty) {
                            match &p.name {
                                // a literal string
                                PropName::Ident(id) | PropName::String(id) => {
                                    elem.push(Type::LiteralString(id.clone().into_boxed_str()))
                                }
                                // a literal integer
                                PropName::Int(i) => elem.push(Type::LiteralInt(*i)),
                                // private properties are not visible
                                PropName::Private(_) => {}
                                // a symbol
                                PropName::Symbol(_) => elem.push(Type::Symbol),
                            }
                        }

                        // add number to union if array
                        if let Type::Array(_) = &ty {
                            elem.push(Type::Number);
                        }

                        // any can be dynamically accessed
                        if let Type::Any = &ty {
                            elem.push(Type::String);
                            elem.push(Type::Number);
                            elem.push(Type::Symbol);
                        }

                        // no properties
                        if elem.is_empty() {
                            return Ok(Type::Undefined);
                        }

                        // return union
                        return Ok(Type::Union(elem.into()));
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
            // typeof operator
            swc::TsType::TsTypeQuery(q) => self.translate_type_query(q),
            swc::TsType::TsTypeRef(r) => {
                let mut ty_args = Vec::new();

                if let Some(params) = &r.type_params {
                    for t in &params.params {
                        ty_args.push(self.translate_type(&t)?)
                    }
                }

                let ty = self.find_type_by_entity_name(&r.type_name)?;

                match ty {
                    MaybeGenericType::Type(ty) => {
                        if !ty_args.is_empty() {
                            return Err(Error::syntax_error(r.span, "expecting 0 type arguments"));
                        }
                        return Ok(ty);
                    }
                    _ => todo!("generics"),
                }
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

                    return Ok(Type::Union(tys.into()));
                }
            },
        }
    }

    pub fn translate_type_query(&mut self, q: &swc::TsTypeQuery) -> Result<Type> {
        let mut ty_args = Vec::new();

        if let Some(param) = &q.type_args {
            for t in &param.params {
                ty_args.push(self.translate_type(&t)?);
            }
        }

        let v = match &q.expr_name {
            swc::TsTypeQueryExpr::TsEntityName(entity) => {
                self.find_value_binding_by_entity_name(entity)?
            }
            swc::TsTypeQueryExpr::Import(i) => {
                return Err(Error::syntax_error(i.span, "import type not supported"))
            }
        };

        match v {
            ValueBinding::Var { ty, .. } | ValueBinding::Using { ty, .. } => {
                if !ty_args.is_empty() {
                    return Err(Error::syntax_error(
                        q.type_args.as_ref().unwrap().span,
                        "Type '' has no signatures for which the type argument list is applicable.",
                    ));
                }

                return Ok(ty);
            }
            ValueBinding::Function(id) => {
                if !ty_args.is_empty() {
                    return Err(Error::syntax_error(
                        q.type_args.as_ref().unwrap().span,
                        "Type '' has no signatures for which the type argument list is applicable.",
                    ));
                }

                let func = self
                    .context
                    .functions
                    .get(&id)
                    .expect("invalid function id");

                return Ok(Type::Function(func.ty().into()));
            }
            ValueBinding::GenericFunction(id) => {
                todo!("generics")
            }
        }
    }

    pub fn translate_literal_types(&mut self, ty: &swc::TsLitType) -> Result<Type> {
        match &ty.lit {
            swc::TsLit::BigInt(i) => {
                if let Some(i) = i.value.to_i128() {
                    Ok(Type::LiteralBigint(i))
                } else {
                    Err(Error::compiler_error(ty.span, "i128 overflow"))
                }
            }
            swc::TsLit::Bool(b) => Ok(Type::LiteralBool(b.value)),
            swc::TsLit::Number(n) => {
                if n.value as i32 as f64 == n.value {
                    Ok(Type::LiteralInt(n.value as i32))
                } else {
                    Ok(Type::LiteralNumber(n.value.into()))
                }
            }
            swc::TsLit::Str(s) => Ok(Type::LiteralString(s.value.as_str().into())),
            // todo: template string
            swc::TsLit::Tpl(_) => Err(Error::compiler_error(
                ty.span,
                "template literal not supported",
            )),
        }
    }

    pub fn translate_literal_object_type(&mut self, ty: &swc::TsTypeLit) -> Result<Type> {
        let mut members = Vec::new();

        for member in &ty.members {
            match member {
                swc::TsTypeElement::TsPropertySignature(p) => {
                    let propname = if p.computed {
                        match self.translate_computed_prop_name(&p.key)? {
                            PropNameOrExpr::PropName(p) => p,
                            PropNameOrExpr::Expr(_, _) => {
                                // not allow dynamic property name
                                return Err(Error::syntax_error(
                                    p.key.span(),
                                    "computed property name not allowed",
                                ));
                            }
                        }
                    } else {
                        match p.key.as_ref() {
                            swc::Expr::Ident(id) => PropName::Ident(id.sym.to_string()),
                            swc::Expr::Lit(swc::Lit::Str(s)) => {
                                PropName::Ident(s.value.to_string())
                            }
                            swc::Expr::PrivateName(p) => PropName::Private(p.id.sym.to_string()),
                            _ => unimplemented!(),
                        }
                    };

                    // not allow generic types
                    if let Some(args) = &p.type_params {
                        return Err(Error::syntax_error(args.span, "generic types not allowed"));
                    }

                    // translate type annotation
                    let mut ty = match &p.type_ann {
                        Some(ann) => self.translate_type(&ann.type_ann)?,
                        None => Type::Any,
                    };

                    // optional type
                    if p.optional {
                        ty = ty.union(Type::Undefined);
                    }

                    // initialiser
                    if let Some(init) = &p.init {
                        // check the expression anyways
                        let (_init_expr, _init_ty) = self.translate_expr(&init, Some(&ty))?;

                        // initialiser not allowed
                        return Err(Error::syntax_error(
                            init.span(),
                            "initialiser not allowed in type declaration",
                        ));
                    };

                    // push property
                    members.push(PropertyDesc {
                        name: propname,
                        ty: ty,
                        readonly: p.readonly,
                    });
                }
                swc::TsTypeElement::TsMethodSignature(m) => {
                    let propname = if m.computed {
                        match self.translate_computed_prop_name(&m.key)? {
                            PropNameOrExpr::PropName(p) => p,
                            PropNameOrExpr::Expr(_, _) => {
                                // not allow dynamic property name
                                return Err(Error::syntax_error(
                                    m.key.span(),
                                    "computed property name not allowed",
                                ));
                            }
                        }
                    } else {
                        match m.key.as_ref() {
                            swc::Expr::Ident(id) => PropName::Ident(id.sym.to_string()),
                            swc::Expr::Lit(swc::Lit::Str(s)) => {
                                PropName::Ident(s.value.to_string())
                            }
                            swc::Expr::PrivateName(p) => PropName::Private(p.id.sym.to_string()),
                            _ => unimplemented!(),
                        }
                    };

                    // not allow generic types
                    if let Some(args) = &m.type_params {
                        return Err(Error::syntax_error(args.span, "generic types not allowed"));
                    }

                    // params
                    let mut params = Vec::new();

                    // translate params
                    for p in &m.params {
                        match p {
                            swc::TsFnParam::Ident(id) => {
                                let mut ty = match &id.type_ann {
                                    Some(ann) => self.translate_type(&ann.type_ann)?,
                                    None => Type::Any,
                                };

                                if id.optional {
                                    ty = ty.union(Type::Undefined);
                                }

                                // push type to param
                                params.push(ty);
                            }
                            _ => {
                                return Err(Error::syntax_error(
                                    p.span(),
                                    "destructive params not supported",
                                ))
                            }
                        }
                    }

                    let return_ty = match &m.type_ann {
                        Some(ann) => self.translate_type(&ann.type_ann)?,
                        None => Type::Undefined,
                    };

                    let mut ty = Type::Function(
                        FuncType {
                            this_ty: Type::Any,
                            params: params,
                            var_arg: false,
                            return_ty: return_ty,
                        }
                        .into(),
                    );

                    if m.optional {
                        ty = ty.union(Type::Undefined);
                    };

                    // push property
                    members.push(PropertyDesc {
                        name: propname,
                        ty: ty,
                        readonly: m.readonly,
                    });
                }
                swc::TsTypeElement::TsGetterSignature(g) => {
                    // todo: getter
                    return Err(Error::syntax_error(g.span, "getter not supported"));
                }
                swc::TsTypeElement::TsSetterSignature(s) => {
                    // todo: setter
                    return Err(Error::syntax_error(s.span, "setter not supported"));
                }
                swc::TsTypeElement::TsIndexSignature(i) => {
                    // todo: index signature
                    return Err(Error::syntax_error(i.span, "index signature not supported"));
                }
                swc::TsTypeElement::TsCallSignatureDecl(c) => {
                    // todo: call signature declare
                    return Err(Error::syntax_error(c.span, "call signature not supported"));
                }
                swc::TsTypeElement::TsConstructSignatureDecl(c) => {
                    // todo: constructor signature
                    return Err(Error::syntax_error(
                        c.span,
                        "constructor signature not supported",
                    ));
                }
            }
        }

        members.sort();

        return Ok(Type::LiteralObject(members.into()));
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

    fn find_namespace_by_entity_name(&self, entity_name: &swc::TsEntityName) -> Result<ModuleId> {
        match entity_name {
            swc::TsEntityName::Ident(id) => match self.context.find_namespace_binding(&id.sym) {
                Some(namespace) => Ok(namespace),
                None => Err(Error::syntax_error(
                    id.span,
                    format!("'{}' is not a namespace", id.sym),
                )),
            },
            swc::TsEntityName::TsQualifiedName(q) => {
                let namespace = self.find_namespace_by_entity_name(&q.left)?;

                let (_, _, namespace) =
                    self.module_export(namespace, &PropName::Ident(q.right.sym.to_string()));

                match namespace {
                    Some(namespace) => Ok(namespace),
                    None => Err(Error::syntax_error(
                        q.right.span,
                        format!("'{}' is not a namespace", q.right.sym),
                    )),
                }
            }
        }
    }

    // lookup binding by entity name
    pub(crate) fn find_type_by_entity_name(
        &self,
        entity_name: &swc::TsEntityName,
    ) -> Result<MaybeGenericType> {
        // match variant of entity name
        match entity_name {
            // an ident entity name
            swc::TsEntityName::Ident(id) => {
                // find binding from the current context
                match self.context.find_class_binding(&id.sym) {
                    Some(ClassBinding::Class(id)) => {
                        // binding is an object type
                        return Ok(MaybeGenericType::Type(Type::Object(*id)));
                    }
                    Some(ClassBinding::Generic(id)) => {
                        // binding is generic
                        return Ok(MaybeGenericType::GenericClass(*id));
                    }
                    None => {}
                };

                match self.context.find_type_binding(&id.sym) {
                    Some(TypeBinding::Enum(e)) => {
                        // binding is enum
                        return Ok(MaybeGenericType::Type(Type::Enum(*e)));
                    }
                    Some(TypeBinding::Interface(id)) => {
                        // binding is interface
                        return Ok(MaybeGenericType::Type(Type::Interface(*id)));
                    }
                    Some(TypeBinding::GenericInterface(id)) => {
                        // binding is generic interface
                        return Ok(MaybeGenericType::GenericInterface(*id));
                    }
                    Some(TypeBinding::TypeAlias(id)) => {
                        // binding is type alias
                        return Ok(MaybeGenericType::Type(Type::Alias(*id)));
                    }
                    Some(TypeBinding::GenericTypeAlias(id)) => {
                        // binding is generic type alias
                        return Ok(MaybeGenericType::GenericTypeAlias(*id));
                    }
                    Some(TypeBinding::Generic(id)) => {
                        // binding is generic
                        return Ok(MaybeGenericType::Generic(*id));
                    }
                    None => {}
                }

                return Err(Error::syntax_error(
                    id.span,
                    format!("undefined identifier '{}'", id.sym),
                ));
            }
            // a chained entity name
            swc::TsEntityName::TsQualifiedName(q) => {
                // find the binding of module
                let namespace = self.find_namespace_by_entity_name(&q.left)?;

                match self
                    .find_type_from_namespace(namespace, &PropName::Ident(q.right.sym.to_string()))
                {
                    Some(ty) => Ok(ty),
                    None => {
                        return Err(Error::syntax_error(
                            q.right.span,
                            format!("'{}' is not a type", q.right.sym),
                        ))
                    }
                }
            }
        }
    }

    fn find_value_binding_by_entity_name(
        &mut self,
        entity_name: &swc::TsEntityName,
    ) -> Result<ValueBinding> {
        match entity_name {
            swc::TsEntityName::Ident(id) => match self.context.find_value_binding(&id.sym) {
                Some(binding) => return Ok(binding.clone()),
                None => {
                    return Err(Error::syntax_error(
                        id.span,
                        format!("undefined identifier '{}'", id.sym),
                    ))
                }
            },
            swc::TsEntityName::TsQualifiedName(q) => {
                let namespace = self.find_namespace_by_entity_name(&q.left)?;

                let (value, _, _) =
                    self.module_export(namespace, &PropName::Ident(q.right.sym.to_string()));

                match value {
                    Some(ModuleValueExport::Function(f)) => return Ok(ValueBinding::Function(f)),
                    Some(ModuleValueExport::GenericFunction(f)) => {
                        return Ok(ValueBinding::GenericFunction(f))
                    }
                    Some(ModuleValueExport::Var(id, ty)) => {
                        return Ok(ValueBinding::Var {
                            writable: true,
                            redeclarable: true,
                            id: id,
                            ty: ty,
                        })
                    }
                    None => {
                        return Err(Error::syntax_error(
                            q.right.span,
                            format!("undefined identifier '{}'", q.right.sym),
                        ))
                    }
                }
            }
        }
    }

    fn find_type_from_namespace(
        &self,
        namespace: ModuleId,
        name: &PropName,
    ) -> Option<MaybeGenericType> {
        let (_value, ty, _namespace) = self.module_export(namespace, name);

        match ty {
            Some(ModuleTypeExport::Class(id)) => Some(MaybeGenericType::Type(Type::Object(id))),
            Some(ModuleTypeExport::Enum(id)) => Some(MaybeGenericType::Type(Type::Enum(id))),
            Some(ModuleTypeExport::Interface(id)) => {
                Some(MaybeGenericType::Type(Type::Interface(id)))
            }
            Some(ModuleTypeExport::TypeAlias(id)) => Some(MaybeGenericType::Type(Type::Alias(id))),
            Some(ModuleTypeExport::GenericClass(id)) => Some(MaybeGenericType::GenericClass(id)),
            Some(ModuleTypeExport::GenericInterface(id)) => {
                Some(MaybeGenericType::GenericInterface(id))
            }
            Some(ModuleTypeExport::GenericTypeAlias(id)) => {
                Some(MaybeGenericType::GenericTypeAlias(id))
            }
            None => None,
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

    pub fn remove_from_union(&self, ty: Type, remove: &Type) -> Type {
        match ty {
            Type::Union(u) => {
                let mut v = Vec::new();

                for i in u.iter() {
                    if i != remove {
                        v.push(i.clone());
                    }
                }

                return Type::Union(v.into());
            }
            _ => return ty,
        }
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
                func.return_ty.clone()
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
                        func.return_ty.clone()
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

        if let Type::Union(u) = ty {
            if u.iter().all(|t| self.type_check(span, t, fulfills).is_ok()) {
                return Ok(());
            }
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
            Type::Array(array_elem) => {
                // a tuple may be converted to array
                if let Type::Tuple(t) = ty {
                    // check if all element of tuple is convertable to element of array
                    if t.iter().all(|e| e == array_elem.as_ref()) {
                        return Ok(());
                    }
                }
            }

            // these types must be strictly obayed
            Type::LiteralObject(_)
            | Type::NamespaceObject(_)
            | Type::Enum(_)
            | Type::Function(_)
            | Type::Map(_, _)
            | Type::Null
            | Type::Promise(_)
            | Type::Regex
            | Type::LiteralBool(_)
            | Type::LiteralNumber(_)
            | Type::LiteralInt(_)
            | Type::LiteralBigint(_)
            | Type::LiteralString(_)
            | Type::Symbol
            | Type::Tuple(_)
            | Type::Undefined => {}
            // bigint
            Type::Bigint => {
                if let Type::LiteralBigint(_) = ty {
                    return Ok(());
                }
            }
            // string
            Type::String => {
                if let Type::LiteralString(_) = ty {
                    return Ok(());
                }
            }
            // number and int are compatable
            Type::Number | Type::Int => {
                if ty == &Type::Number || ty == &Type::Int {
                    return Ok(());
                }
                if let Type::LiteralInt(_) = ty {
                    return Ok(());
                }
                if let Type::LiteralNumber(_) = ty {
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
                        if func.params.len() == 0 && iter_elem.as_ref() == &func.return_ty {
                            return Ok(());
                        }
                    }
                }
            }
        }
        return Err(Error::type_error(span, fulfills.clone(), ty.clone(), ""));
    }
}
