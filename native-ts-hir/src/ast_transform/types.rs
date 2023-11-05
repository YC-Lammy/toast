use native_js_common::rc::Rc;

use native_js_common::error::Error;

use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    context::Binding,
    untyped_hir::{FunctionType, GenericId, GenericParam, Type},
};

use super::{Result, Translater};

impl Translater {
    pub fn translate_generic_params(
        &mut self,
        params: &swc::TsTypeParamDecl,
    ) -> Result<Vec<GenericParam>> {
        let mut generics = Vec::new();

        for p in &params.params {
            let default = if let Some(d) = &p.default {
                Some(self.translate_ty(d)?)
            } else {
                None
            };

            let constrain = if let Some(c) = &p.constraint {
                Some(self.translate_ty(c)?)
            } else {
                None
            };

            let gid = GenericId::new();

            generics.push(GenericParam {
                name: p.name.sym.to_string(),
                span: p.span,
                id: gid,
                constrain: constrain,
                default: default,
            });

            if !self.context.add_generic(&p.name.sym, gid) {
                return Err(Error::syntax_error(p.name.span, "duplicated identifier"));
            };
        }

        return Ok(generics);
    }

    pub fn translate_type_expr(&mut self, expr: &swc::Expr, ty_args: Vec<Type>) -> Result<Type> {
        match expr {
            swc::Expr::Ident(id) => {
                if let Some(binding) = self.context.find(&id.sym).cloned() {
                    return self.binding_with_type_args(id.span, &binding, ty_args);
                }
            }
            swc::Expr::TsInstantiation(i) => {
                if !ty_args.is_empty() {
                    todo!()
                }

                let mut ty_args = Vec::new();

                for i in i.type_args.params.iter() {
                    let t = self.translate_ty(i)?;
                    ty_args.push(t);
                }

                return self.translate_type_expr(&i.expr, ty_args);
            }
            _ => {}
        }
        return Err(Error::syntax_error(expr.span(), "invalid type expression"));
    }

    pub fn translate_ty(&mut self, ty: &swc::TsType) -> Result<Type> {
        match ty {
            swc::TsType::TsArrayType(a) => {
                let ty = self.translate_ty(&a.elem_type)?;
                return Ok(Type::Array(Box::new(ty)));
            }
            swc::TsType::TsConditionalType(c) => {
                return Err(Error::syntax_error(
                    c.span,
                    "conditional type not supported",
                ))
            }
            swc::TsType::TsFnOrConstructorType(f) => match f {
                swc::TsFnOrConstructorType::TsConstructorType(c) => {
                    return Err(Error::syntax_error(c.span, "constructor type not allowed"))
                }
                swc::TsFnOrConstructorType::TsFnType(f) => {
                    if f.type_params.is_some() {
                        return Err(Error::syntax_error(
                            f.span,
                            "type params not allowed in function type definition",
                        ));
                    }
                    Ok(Type::Function {
                        type_args: Box::new([]),
                        func: self.translate_fn_ty(
                            &f.params,
                            f.type_params.as_deref(),
                            Some(&f.type_ann),
                        )?,
                    })
                }
            },
            swc::TsType::TsImportType(i) => {
                return Err(Error::syntax_error(i.span, "import type not supported"))
            }
            swc::TsType::TsIndexedAccessType(i) => {
                return Err(Error::syntax_error(
                    i.span,
                    "import access type not supported",
                ))
            }
            swc::TsType::TsInferType(i) => {
                return Err(Error::syntax_error(i.span, "infer type not supported"))
            }
            swc::TsType::TsKeywordType(w) => return Ok(self.translate_keyword_type(w)),
            swc::TsType::TsLitType(l) => {
                return Err(Error::syntax_error(l.span, "literal type not allowed"))
            }
            swc::TsType::TsMappedType(m) => {
                return Err(Error::syntax_error(m.span, "mapped type not allowed"))
            }
            swc::TsType::TsOptionalType(o) => {
                let ty = self.translate_ty(&o.type_ann)?;
                return Ok(ty.optional());
            }
            swc::TsType::TsParenthesizedType(p) => return self.translate_ty(&p.type_ann),
            swc::TsType::TsRestType(r) => {
                return Err(Error::syntax_error(r.span, "rest type not allowed"))
            }
            swc::TsType::TsThisType(_) => return Ok(Type::This),
            swc::TsType::TsTupleType(t) => {
                return Err(Error::syntax_error(t.span, "tuple types not supported"))
            }
            swc::TsType::TsTypeLit(t) => {
                return Err(Error::syntax_error(t.span, "type literal not allowed"))
            }
            swc::TsType::TsTypeOperator(o) => {
                return Err(Error::syntax_error(o.span, "type operator not supported"))
            }
            swc::TsType::TsTypePredicate(p) => {
                return Err(Error::syntax_error(p.span, "type predicate not supported"))
            }
            swc::TsType::TsTypeQuery(q) => {
                return Err(Error::syntax_error(q.span, "type query not supported"))
            }
            swc::TsType::TsTypeRef(r) => self.translate_type_ref(r),
            swc::TsType::TsUnionOrIntersectionType(u) => match u {
                swc::TsUnionOrIntersectionType::TsIntersectionType(_) => {
                    todo!()
                }
                swc::TsUnionOrIntersectionType::TsUnionType(u) => {
                    let mut v = Vec::new();
                    for t in &u.types{
                        v.push(self.translate_ty(t)?);
                    }
                    return Ok(Type::Union(v))
                }
            },
        }
    }

    pub fn translate_fn_ty(
        &mut self,
        params: &[swc::TsFnParam],
        type_params: Option<&swc::TsTypeParamDecl>,
        type_ann: Option<&swc::TsTypeAnn>,
    ) -> Result<Rc<FunctionType>> {
        if let Some(ty_params) = type_params {
            return Err(Error::syntax_error(
                ty_params.span,
                "function type must be concrete",
            ));
        }

        let mut this_ty = Type::Any;
        let mut param_ty = Vec::new();

        for p in params {
            if let Some(p) = p.as_ident() {
                if let Some(ann) = &p.type_ann {
                    let mut ty = self.translate_ty(&ann.type_ann)?;

                    if p.optional {
                        ty = ty.optional();
                    }

                    if &p.id.sym == "this" {
                        this_ty = ty;
                    } else {
                        param_ty.push(ty);
                    }
                } else {
                    return Err(Error::syntax_error(p.span, "missing type annotation"));
                }
            } else {
                return Err(Error::syntax_error(
                    p.span(),
                    "function param must be ident",
                ));
            }
        }

        let return_ty = if let Some(ann) = type_ann {
            self.translate_ty(&ann.type_ann)?
        } else {
            Type::Undefined
        };

        return Ok(Rc::new(FunctionType {
            visit_fingerprint: 0,
            is_definite: true,
            this_ty: this_ty,
            generics: Vec::new(),
            params: param_ty,
            return_ty: return_ty,
        }));
    }

    pub fn translate_keyword_type(&mut self, t: &swc::TsKeywordType) -> Type {
        match t.kind {
            swc::TsKeywordTypeKind::TsAnyKeyword => Type::Any,
            swc::TsKeywordTypeKind::TsBigIntKeyword => Type::BigInt,
            swc::TsKeywordTypeKind::TsBooleanKeyword => Type::Bool,
            swc::TsKeywordTypeKind::TsIntrinsicKeyword => Type::unknown(t.span),
            swc::TsKeywordTypeKind::TsNeverKeyword => Type::Undefined,
            swc::TsKeywordTypeKind::TsNullKeyword => Type::Null,
            swc::TsKeywordTypeKind::TsNumberKeyword => Type::Number,
            swc::TsKeywordTypeKind::TsObjectKeyword => Type::Any,
            swc::TsKeywordTypeKind::TsStringKeyword => Type::String,
            swc::TsKeywordTypeKind::TsSymbolKeyword => Type::Symbol,
            swc::TsKeywordTypeKind::TsUndefinedKeyword => Type::Undefined,
            swc::TsKeywordTypeKind::TsUnknownKeyword => Type::unknown(t.span),
            swc::TsKeywordTypeKind::TsVoidKeyword => Type::Undefined,
        }
    }

    fn find_entity_name(&mut self, name: &swc::TsEntityName) -> Option<Binding> {
        match name {
            swc::TsEntityName::Ident(id) => self.context.find(&id.sym).cloned(),
            swc::TsEntityName::TsQualifiedName(q) => {
                let b = self.find_entity_name(&q.left)?;

                match b {
                    Binding::NameSpace(m) => return self.find_module_export(m, &q.right.sym),
                    _ => None,
                }
            }
        }
    }

    pub fn translate_type_ref(&mut self, tr: &swc::TsTypeRef) -> Result<Type> {
        let mut type_args = Vec::new();

        if let Some(params) = &tr.type_params {
            for param in &params.params {
                type_args.push(self.translate_ty(&param)?);
            }
        }
        if let Some(binding) = self.find_entity_name(&tr.type_name) {
            return self.binding_with_type_args(tr.span, &binding, type_args);
        }

        fn f(buf: &mut String, name: &swc::TsEntityName) {
            match name {
                swc::TsEntityName::Ident(id) => {
                    buf.push_str(&id.sym);
                }
                swc::TsEntityName::TsQualifiedName(q) => {
                    f(buf, &q.left);
                    buf.push_str(".");
                    buf.push_str(&q.right.sym);
                }
            };
        }

        let mut buf = String::new();
        f(&mut buf, &tr.type_name);

        return Err(Error::syntax_error(
            tr.span,
            format!("cannot reference type '{}'", buf),
        ));
    }

    fn binding_with_type_args(
        &mut self,
        sp: Span,
        binding: &Binding,
        ty_args: Vec<Type>,
    ) -> Result<Type> {
        match binding {
            Binding::Class(c) => {
                let min_generics = c.minimum_generics();
                if ty_args.len() < min_generics {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "class '{}' expected {} type arguments, {} were given",
                            c.name,
                            min_generics,
                            ty_args.len()
                        ),
                    ));
                }

                if c.is_definite && ty_args.len() > c.generics.len() {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "class '{}' expected {} type arguments, {} were given",
                            c.name,
                            c.generics.len(),
                            ty_args.len()
                        ),
                    ));
                };

                return Ok(Type::Class {
                    span: sp,
                    type_args: ty_args.into(),
                    class: c.clone(),
                });
            }
            Binding::Enum(e) => {
                if !ty_args.is_empty() {
                    return Err(Error::syntax_error(
                        sp,
                        "Enum type expected 0 type arguments",
                    ));
                }
                return Ok(Type::Enum(e.clone()));
            }
            Binding::Generic(g) => {
                if !ty_args.is_empty() {
                    return Err(Error::syntax_error(
                        sp,
                        "Generic type expected 0 type arguments",
                    ));
                }
                return Ok(Type::Generic(*g));
            }
            Binding::Interface(i) => {
                let min_generics = i.minimum_generics();
                if ty_args.len() < min_generics {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "interface '{}' expected {} type arguments, {} were given",
                            i.name,
                            min_generics,
                            ty_args.len()
                        ),
                    ));
                }

                if i.is_definite && ty_args.len() > i.generics.len() {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "interface '{}' expected {} type arguments, {} were given",
                            i.name,
                            i.generics.len(),
                            ty_args.len()
                        ),
                    ));
                }

                return Ok(Type::Interface {
                    span: sp,
                    type_args: ty_args.into(),
                    interface: i.clone(),
                });
            }
            Binding::TypeAlias(a) => {
                let min_generics = a.minimum_generics();
                if ty_args.len() < min_generics {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "alias '{}' expected {} type arguments, {} were given",
                            a.name,
                            min_generics,
                            ty_args.len()
                        ),
                    ));
                }

                if a.is_definite && ty_args.len() > a.generics.len() {
                    return Err(Error::syntax_error(
                        sp,
                        format!(
                            "alias '{}' expected {} type arguments, {} were given",
                            a.name,
                            a.generics.len(),
                            ty_args.len()
                        ),
                    ));
                };

                return Ok(Type::Alias {
                    span: sp,
                    type_args: ty_args.into(),
                    alias: a.clone(),
                });
            }
            _ => return Err(Error::syntax_error(sp, "binding is not a type")),
        }
    }
}
