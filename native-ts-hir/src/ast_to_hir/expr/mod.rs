use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, Type};
use crate::util::OR;
use crate::PropName;

use super::Transformer;
use super::ValueBinding;

mod array;
mod arrow;
mod assign;
mod r#await;
mod bin;
mod call;
mod class;
mod cond;
mod function;
mod instantiation;
mod lit;
mod member;
mod new;
mod object_literal;
mod optchain;
mod super_prop;
mod tpl;
mod unary;
mod update;

pub type Result<T> = std::result::Result<T, Error>;

pub enum MaybeTranslatedExpr<'a> {
    NotTranslated(&'a swc::Expr),
    Translated(Expr, Type),
}

impl<'a> MaybeTranslatedExpr<'a> {
    pub fn translate(
        self,
        transformer: &mut Transformer,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match self {
            Self::NotTranslated(e) => return transformer.translate_expr(e, expected_ty),
            Self::Translated(e, ty) => Ok((e, ty)),
        }
    }
}

impl Transformer {
    /// entry for translateing expressions
    pub fn translate_expr(
        &mut self,
        expr: &swc::Expr,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        let (mut e, ty) = match expr {
            // array expression
            swc::Expr::Array(a) => self.translate_array_expr(a, expected_ty)?,
            // arrow expression
            swc::Expr::Arrow(a) => self.translate_arrow_expr(a, expected_ty)?,
            // assignment expression
            swc::Expr::Assign(a) => self.translate_assign_expr(a)?,
            // await expression
            swc::Expr::Await(a) => self.translate_await_expr(a, expected_ty)?,
            // binary operation
            swc::Expr::Bin(b) => self.translate_bin_expr(b)?,
            // function call expression
            swc::Expr::Call(c) => self.translate_call_expr(c)?,
            // class exression
            swc::Expr::Class(c) => self.translate_class_expr(c)?,
            // conditional expression
            swc::Expr::Cond(cond) => self.translate_cond_expr(cond)?,
            // function expression
            swc::Expr::Fn(f) => self.translate_func_expr(f)?,
            // variable load
            swc::Expr::Ident(id) => self.translate_var_load(id)?,
            // this
            swc::Expr::This(t) => (Expr::This(t.span), self.this_ty.clone()),
            // object literal
            swc::Expr::Object(o) => self.translate_object_lit_expr(o, expected_ty)?,
            // unary operation
            swc::Expr::Unary(u) => self.translate_unary_expr(u)?,
            // update expression
            swc::Expr::Update(u) => self.translate_update_expr(u)?,
            // member expression
            swc::Expr::Member(m) => self.translate_member_expr(m)?,
            // super.prop expression
            swc::Expr::SuperProp(s) => self.translate_super_prop_expr(s)?,
            // class construction expression
            swc::Expr::New(n) => self.translate_new_expr(n)?,
            // sequence expression
            swc::Expr::Seq(s) => self.translate_seq_expr(s, expected_ty)?,
            // literal expression
            swc::Expr::Lit(l) => self.translate_lit_expr(l, expected_ty)?,
            // template literal exprssion
            swc::Expr::Tpl(t) => self.translate_tpl_expr(t)?,
            // tagged template literal
            swc::Expr::TaggedTpl(t) => self.translate_tagged_tpl_expr(t)?,
            // yield expression
            swc::Expr::Yield(y) => self.translate_yield_expr(y)?,
            // paren expression
            swc::Expr::Paren(p) => self.translate_expr(&p.expr, expected_ty)?,
            // meta property expression
            // todo: meta prop
            swc::Expr::MetaProp(m) => {
                return Err(Error::syntax_error(m.span, "meta properties not supported"))
            }
            // not valid
            swc::Expr::PrivateName(p) => {
                return Err(Error::syntax_error(p.span, "invalid expression"))
            }
            // invalid expression
            swc::Expr::Invalid(i) => return Err(Error::syntax_error(i.span, "invalid expression")),
            // optchain expression
            swc::Expr::OptChain(o) => self.translate_optchain_expr(o)?,
            // typescript type cast expression
            swc::Expr::TsAs(a) => self.translate_ts_as_expr(a)?,
            // typescript const assert expression
            swc::Expr::TsConstAssertion(c) => {
                // does nothing for now
                self.translate_expr(&c.expr, expected_ty)?
            }
            // typescript instantiation
            swc::Expr::TsInstantiation(i) => self.translate_instantiation_expr(i)?,
            // typescript non null
            swc::Expr::TsNonNull(n) => self.translate_non_null_expr(n)?,
            // typescript satisfies expression
            swc::Expr::TsSatisfies(s) => self.translate_ts_satisfies_expr(s)?,
            // typescript assertion expression
            swc::Expr::TsTypeAssertion(t) => self.translate_ts_type_assert_expr(t)?,

            swc::Expr::JSXElement(_)
            | swc::Expr::JSXEmpty(_)
            | swc::Expr::JSXFragment(_)
            | swc::Expr::JSXMember(_)
            | swc::Expr::JSXNamespacedName(_) => unimplemented!(),
        };

        if let Some(expected) = expected_ty {
            self.type_check(expr.span(), &ty, expected)?;

            if !ty.eq(expected) {
                self.cast(&mut e, &ty, expected)
            }

            return Ok((e, expected.clone()));
        }

        return Ok((e, ty));
    }

    fn translate_var_load(&mut self, ident: &swc::Ident) -> Result<(Expr, Type)> {
        match self.context.find_namespace_or_value_binding(&ident.sym) {
            Some(OR::A(namespace)) => {
                return Ok((
                    Expr::NamespaceObject(namespace),
                    Type::NamespaceObject(namespace),
                ))
            }
            Some(OR::B(ValueBinding::Var { id, ty, .. }))
            | Some(OR::B(ValueBinding::Using { id, ty, .. })) => {
                return Ok((
                    Expr::VarLoad {
                        span: ident.span,
                        variable: *id,
                    },
                    ty.clone(),
                ));
            }
            Some(OR::B(ValueBinding::Function(f))) => {
                // copy the id to avoid borrowing self
                let id = *f;
                // get the functio type
                let ty = self
                    .context
                    .functions
                    .get(&id)
                    .expect("invalid function id")
                    .ty();
                // return expression
                return Ok((Expr::Function(id), Type::Function(ty.into())));
            }
            Some(OR::B(ValueBinding::GenericFunction(_f))) => {
                return Err(Error::syntax_error(ident.span, "missing type arguments"))
            }
            None => {
                if self.context.has_binding(&ident.sym) {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("identifier '{}' is not a variable", ident.sym),
                    ));
                } else {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("undefined identifier '{}'", ident.sym),
                    ));
                }
            }
        }
    }

    fn translate_yield_expr(&mut self, expr: &swc::YieldExpr) -> Result<(Expr, Type)> {
        let arg = if let Some(expr) = &expr.arg {
            let r = self.return_ty.clone();
            let (e, _) = self.translate_expr(expr, Some(&r))?;
            e
        } else {
            self.type_check(expr.span, &Type::Undefined, &self.return_ty)?;
            Expr::Undefined
        };
        // for now use undefined
        Ok((Expr::Yield(Box::new(arg)), Type::Undefined))
    }

    fn translate_seq_expr(
        &mut self,
        s: &swc::SeqExpr,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        let mut ty = Type::Undefined;
        let mut seq = Vec::new();

        for (i, e) in s.exprs.iter().enumerate() {
            // only give in expected if last
            let expected_ty = if i == s.exprs.len() - 1 {
                expected_ty
            } else {
                None
            };
            let (expr, t) = self.translate_expr(&e, expected_ty)?;

            seq.push(expr);
            ty = t;
        }

        if seq.is_empty() {
            return Ok((Expr::Undefined, Type::Undefined));
        }

        return Ok((Expr::Seq { seq: seq }, ty));
    }

    pub fn translate_ts_as_expr(&mut self, expr: &swc::TsAsExpr) -> Result<(Expr, Type)> {
        let ty = self.translate_type(&expr.type_ann)?;
        // translate expr will handle the cast
        let (expr, ty) = self.translate_expr(&expr.expr, Some(&ty))?;
        Ok((expr, ty))
    }

    pub fn translate_non_null_expr(&mut self, expr: &swc::TsNonNullExpr) -> Result<(Expr, Type)> {
        let (e, mut ty) = self.translate_expr(&expr.expr, None)?;

        if ty == Type::Undefined || ty == Type::Null {
            return Err(Error::syntax_error(
                expr.span,
                "non null assertion cannot be applied to null types",
            ));
        }

        if let Type::Union(u) = &ty {
            let mut v = Vec::new();
            for ty in u.iter() {
                if ty != &Type::Undefined && ty != &Type::Null {
                    v.push(ty.clone());
                }
            }
            ty = Type::Union(v.into());
        }

        Ok((Expr::AssertNonNull(Box::new(e)), ty))
    }

    pub fn translate_ts_satisfies_expr(&mut self, expr: &swc::TsSatisfiesExpr) -> Result<(Expr, Type)>{
        let ty = self.translate_type(&expr.type_ann)?;
        let (e, expr_ty) = self.translate_expr(&expr.expr, None)?;
        // check satisfies
        self.type_check(expr.span, &expr_ty, &ty)?;

        Ok((e, expr_ty))
    }

    pub fn translate_ts_type_assert_expr(&mut self, expr: &swc::TsTypeAssertion) -> Result<(Expr, Type)>{
        let ty = self.translate_type(&expr.type_ann)?;
        // translate expr will handle the cast
        let (expr, ty) = self.translate_expr(&expr.expr, Some(&ty))?;
        Ok((expr, ty))
    }

    pub(super) fn translate_prop_name(&mut self, name: &swc::PropName) -> Result<PropNameOrExpr> {
        match name {
            swc::PropName::BigInt(b) => Ok(PropNameOrExpr::PropName(crate::PropName::String(
                b.value.to_string(),
            ))),
            swc::PropName::Ident(id) => Ok(PropNameOrExpr::PropName(crate::PropName::Ident(
                id.sym.to_string(),
            ))),
            swc::PropName::Num(n) => {
                if n.value as i32 as f64 == n.value {
                    Ok(PropNameOrExpr::PropName(crate::PropName::Int(
                        n.value as i32,
                    )))
                } else {
                    Ok(PropNameOrExpr::PropName(crate::PropName::String(
                        n.value.to_string(),
                    )))
                }
            }
            swc::PropName::Str(s) => Ok(PropNameOrExpr::PropName(crate::PropName::String(
                s.value.to_string(),
            ))),
            swc::PropName::Computed(c) => self.translate_computed_prop_name(&c.expr),
        }
    }

    pub(super) fn translate_computed_prop_name(
        &mut self,
        expr: &swc::Expr,
    ) -> Result<PropNameOrExpr> {
        match expr {
            swc::Expr::PrivateName(n) => {
                return Ok(PropNameOrExpr::PropName(PropName::Private(
                    n.id.sym.to_string(),
                )))
            }
            swc::Expr::Lit(l) => {
                match l {
                    swc::Lit::BigInt(b) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            b.value.to_string(),
                        )))
                    }
                    swc::Lit::Bool(b) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            b.value.to_string(),
                        )))
                    }
                    swc::Lit::JSXText(j) => {
                        return Err(Error::syntax_error(j.span, "JSXText is not allowed"))
                    }
                    swc::Lit::Null(_) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            "null".to_string(),
                        )))
                    }
                    swc::Lit::Num(n) => {
                        if n.value as i32 as f64 == n.value {
                            return Ok(PropNameOrExpr::PropName(crate::PropName::Int(
                                n.value as i32,
                            )));
                        } else {
                            return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                                n.value.to_string(),
                            )));
                        };
                    }
                    swc::Lit::Regex(r) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(format!(
                            "/{}/{}",
                            r.exp, r.flags
                        ))))
                    }
                    swc::Lit::Str(s) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            s.value.to_string(),
                        )))
                    }
                };
            }
            swc::Expr::Member(mem) => match mem.obj.as_ref() {
                swc::Expr::Ident(id) => {
                    if id.sym.as_ref() == "Symbol" {
                        if let Some(prop) = mem.prop.as_ident() {
                            match prop.sym.as_ref() {
                                "asyncIterator" => {
                                    return Ok(PropNameOrExpr::PropName(crate::PropName::Symbol(
                                        crate::Symbol::AsyncIterator,
                                    )))
                                }
                                "hasInstance" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::HasInstance,
                                    )))
                                }
                                "isConcatSpreadable" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::IsConcatSpreadable,
                                    )))
                                }
                                "iterator" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Iterator,
                                    )))
                                }
                                "match" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Match,
                                    )))
                                }
                                "matchAll" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::MatchAll,
                                    )))
                                }
                                "replace" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Replace,
                                    )))
                                }
                                "search" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Search,
                                    )))
                                }
                                "species" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Species,
                                    )))
                                }
                                "split" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Split,
                                    )))
                                }
                                "toStringTag" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::ToStringTag,
                                    )))
                                }
                                "unscopables" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Unscopables,
                                    )))
                                }
                                "dispose" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Dispose,
                                    )))
                                }
                                "asyncDispose" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::AsyncDispose,
                                    )))
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        };

        let (expr, ty) = self.translate_expr(expr, None)?;

        return Ok(PropNameOrExpr::Expr(Box::new(expr), ty));
    }
}
