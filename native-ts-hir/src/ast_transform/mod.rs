mod class;
mod expr;
mod function;
mod module;
mod stmt;
mod types;

use swc_common::Span;
use swc_common::Spanned;
use swc_ecmascript::ast as swc;

use native_js_common::error::Error;

use crate::VarKind;

use crate::context::Context;
use crate::untyped_hir::visit::Visit;
use crate::untyped_hir::Type;

type Result<T> = core::result::Result<T, Error<Span>>;

pub struct Translater {
    pub context: Context,
}

impl Translater {
    pub fn new() -> Self {
        Self {
            context: Context::new(),
        }
    }

    pub fn hoist<'a, I: Iterator<Item = &'a swc::Stmt> + Clone>(&mut self, stmts: I) -> Result<()> {
        // hoist types
        for stmt in stmts.clone() {
            match stmt {
                swc::Stmt::Decl(decl) => {
                    self.hoist_ty_decl(decl)?;
                }
                _ => {}
            }
        }

        // hoist variables
        for stmt in stmts {
            match stmt {
                swc::Stmt::Decl(decl) => {
                    self.hoist_var_decl(decl)?;
                }
                _ => {}
            }
        }

        return Ok(());
    }

    pub fn hoist_ty_decl(&mut self, decl: &swc::Decl) -> Result<()> {
        match decl {
            swc::Decl::Class(c) => {
                if !self.context.add_class(&c.ident.sym) {
                    return Err(Error::syntax_error(c.ident.span, "duplicated identifier"));
                };
            }
            swc::Decl::Fn(f) => {
                if !self.context.add_function(&f.ident.sym) {
                    return Err(Error::syntax_error(f.ident.span, "duplicated identifier"));
                }
            }
            swc::Decl::TsEnum(e) => {
                if !self.context.add_enum(&e.id.sym) {
                    return Err(Error::syntax_error(e.id.span, "duplicated identifier"));
                }
            }
            swc::Decl::TsInterface(i) => {
                if !self.context.add_interface(&i.id.sym) {
                    return Err(Error::syntax_error(i.id.span, "duplicated identifier"));
                }
            }
            swc::Decl::TsModule(_) => {}
            swc::Decl::TsTypeAlias(a) => {
                if !self.context.add_alias(&a.id.sym) {
                    return Err(Error::syntax_error(a.id.span, "duplicated identifier"));
                }
            }
            _ => {}
        }
        return Ok(());
    }

    pub fn hoist_var_decl(&mut self, decl: &swc::Decl) -> Result<()> {
        match decl {
            swc::Decl::Var(v) => {
                let kind = match v.kind {
                    swc::VarDeclKind::Const => VarKind::Const,
                    swc::VarDeclKind::Let => VarKind::Let,
                    swc::VarDeclKind::Var => VarKind::Var,
                };

                for d in &v.decls {
                    self.hoist_pat(kind, &d.name, None)?;
                }
            }
            swc::Decl::Using(u) => {
                let kind = if u.is_await {
                    VarKind::AwaitUsing
                } else {
                    VarKind::Using
                };

                for d in &u.decls {
                    self.hoist_pat(kind, &d.name, None)?;
                }
            }
            _ => {}
        };

        return Ok(());
    }

    pub fn hoist_pat(
        &mut self,
        kind: VarKind,
        pat: &swc::Pat,
        type_ann: Option<Type>,
    ) -> Result<()> {
        match pat {
            swc::Pat::Ident(id) => {
                let mut ty = 
                // annotation from outer expression
                if let Some(ann) = type_ann{
                    // should not be annotated if annotated by outer
                    if id.type_ann.is_some(){
                        return Err(Error::syntax_error(id.span, "duplicated type annotation"))
                    }
                    ann
                } else{
                    // type annotation present
                    if let Some(ann) = &id.type_ann {
                        self.translate_ty(&ann.type_ann)?
                    } else {
                        // unknown
                        Type::unknown(id.span)
                    }
                };

                if id.optional {
                    ty = ty.optional();
                }

                let is_err = match kind {
                    VarKind::AwaitUsing => self.context.add_await_using(&id.id.sym, ty),
                    VarKind::Using => self.context.add_using(&id.id.sym, ty),
                    VarKind::Const => self.context.add_const(&id.id.sym, ty),
                    VarKind::Let => self.context.add_let(&id.id.sym, ty),
                    VarKind::Var => self.context.add_var(&id.id.sym, ty),
                }
                .is_none();

                if is_err {
                    return Err(Error::syntax_error(
                        id.span,
                        format!("duplicated identifier '{}'", id.id.sym),
                    ));
                }
            }

            swc::Pat::Array(a) => {
                let elem_ty = if let Some(ty) = &type_ann {
                    if let Type::Array(ty) = &ty {
                        ty.as_ref().clone()
                    } else {
                        return Err(Error::syntax_error(
                            a.span,
                            "expected array type annotation",
                        ));
                    }
                } else {
                    // all elements must have the same type
                    Type::unknown(a.span)
                };

                for e in &a.elems {
                    if let Some(p) = e {
                        self.hoist_pat(kind, p, Some(elem_ty.clone()))?;
                    }
                }
            }
            swc::Pat::Assign(a) => {
                self.hoist_pat(kind, &a.left, type_ann)?;
            }

            swc::Pat::Object(_o) => {
                return Err(Error::syntax_error(
                    pat.span(),
                    "destructive assignment not supported",
                ))
            }
            swc::Pat::Rest(r) => {
                // todo: rest pattern
                return Err(Error::syntax_error(
                    r.dot3_token,
                    "rest pattern not supported",
                ));
            }
            swc::Pat::Invalid(n) => return Err(Error::syntax_error(n.span, "invalid pattern")),
            swc::Pat::Expr(e) => return Err(Error::syntax_error(e.span(), "invalid pattern")),
        };

        return Ok(());
    }

    pub fn translate_prop_name(&mut self, name: &swc::PropName) -> Result<crate::PropName> {
        match name {
            swc::PropName::BigInt(b) => Ok(crate::PropName::String(b.value.to_string())),
            swc::PropName::Ident(id) => Ok(crate::PropName::Ident(id.sym.to_string())),
            swc::PropName::Num(n) => {
                if n.value as i32 as f64 == n.value {
                    Ok(crate::PropName::Int(n.value as i32))
                } else {
                    Ok(crate::PropName::String(n.value.to_string()))
                }
            }
            swc::PropName::Str(s) => Ok(crate::PropName::String(s.value.to_string())),
            swc::PropName::Computed(c) => self.translate_computed_prop_name(&c.expr),
        }
    }

    fn translate_computed_prop_name(&self, expr: &swc::Expr) -> Result<crate::PropName> {
        match expr {
            swc::Expr::PrivateName(n) => return Ok(crate::PropName::Private(n.id.sym.to_string())),
            swc::Expr::Lit(l) => {
                match l {
                    swc::Lit::BigInt(b) => return Ok(crate::PropName::String(b.value.to_string())),
                    swc::Lit::Bool(b) => return Ok(crate::PropName::String(b.value.to_string())),
                    swc::Lit::JSXText(j) => {
                        return Err(Error::syntax_error(j.span, "JSXText is not allowed"))
                    }
                    swc::Lit::Null(_) => return Ok(crate::PropName::String("null".to_string())),
                    swc::Lit::Num(n) => {
                        if n.value as i32 as f64 == n.value {
                            return Ok(crate::PropName::Int(n.value as i32));
                        } else {
                            return Ok(crate::PropName::String(n.value.to_string()));
                        };
                    }
                    swc::Lit::Regex(r) => {
                        return Ok(crate::PropName::String(format!("/{}/{}", r.exp, r.flags)))
                    }
                    swc::Lit::Str(s) => return Ok(crate::PropName::String(s.value.to_string())),
                };
            }
            swc::Expr::Member(mem) => match mem.obj.as_ref() {
                swc::Expr::Ident(id) => {
                    if id.sym.as_ref() == "Symbol" {
                        if let Some(prop) = mem.prop.as_ident() {
                            match prop.sym.as_ref() {
                                "asyncIterator" => {
                                    return Ok(crate::PropName::Symbol(
                                        crate::Symbol::AsyncIterator,
                                    ))
                                }
                                "hasInstance" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::HasInstance))
                                }
                                "isConcatSpreadable" => {
                                    return Ok(crate::PropName::Symbol(
                                        crate::Symbol::IsConcatSpreadable,
                                    ))
                                }
                                "iterator" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Iterator))
                                }
                                "match" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Match))
                                }
                                "matchAll" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::MatchAll))
                                }
                                "replace" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Replace))
                                }
                                "search" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Search))
                                }
                                "species" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Species))
                                }
                                "split" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Split))
                                }
                                "toStringTag" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::ToStringTag))
                                }
                                "unscopables" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Unscopables))
                                }
                                "dispose" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Dispose))
                                }
                                "asyncDispose" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::AsyncDispose))
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

        return Err(Error::syntax_error(
            expr.span(),
            "computed property names not allowed",
        ));
    }
}

#[test]
fn translate() {
    let s = include_str!("../../binary.ts");

    let parser = native_ts_parser::Parser::new();

    let program = parser
        .parse_str("main".to_string(), s.to_string())
        .expect("error parsing source");

    for (_, module) in &program.modules {
        let mut t = Translater::new();

        let re = t.hoist(module.module.body.iter().filter_map(|i| i.as_stmt()));

        for i in &module.module.body {
            if let Some(s) = i.as_stmt() {
                let re = t.translate_stmt(s, None);
                println!("{:#?}", re);
            }
        }

        let mut main = t.context.end_function();

        println!("{:#?}", main);

        let mut visitor = crate::passes::unknown::UnknownFinder::default();
        main.visit(&mut visitor).expect("");

        main.visit(&mut visitor.resolver()).expect("");
    }
}
