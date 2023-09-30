//! resolves dependencies, package management and file caching

use swc_atoms::JsWord;
use swc_bundler::ModuleData;
use swc_common::SyntaxContext;
use swc_common::{sync::Lrc, FileName, SourceMap, Span, Spanned};
use swc_ecmascript::ast::*;
use swc_ecmascript::parser::{parse_file_as_module, EsConfig, Syntax, TsConfig};

use crate::error::Error;

pub struct PathLoader {
    pub cm: Lrc<SourceMap>,
    progress: indicatif::MultiProgress,
}

impl PathLoader {
    pub fn new(cm: Lrc<SourceMap>) -> Self {
        let progress = indicatif::MultiProgress::new();
        let _ = progress.println("resolving modules...");
        Self { cm, progress }
    }
}

impl swc_bundler::Load for PathLoader {
    fn load(&self, file: &FileName) -> Result<swc_bundler::ModuleData, anyhow::Error> {
        match file {
            FileName::Real(f) => {
                let fm = self.cm.load_file(f)?;

                let progress = indicatif::ProgressBar::new_spinner();
                progress.set_message(f.to_string_lossy().to_string());

                self.progress.add(progress.clone());

                let syntax = if f.extension().is_some_and(|s| s == "js") {
                    Syntax::Es(EsConfig {
                        jsx: false,
                        fn_bind: true,
                        decorators: true,
                        decorators_before_export: true,
                        export_default_from: true,
                        import_assertions: true,
                        allow_super_outside_method: false,
                        allow_return_outside_function: true,
                        auto_accessors: true,
                        using_decl: false,
                    })
                } else if f.extension().is_some_and(|s| s == "ts") {
                    Syntax::Typescript(TsConfig {
                        tsx: false,
                        decorators: true,
                        dts: false,
                        no_early_errors: false,
                        disallow_ambiguous_jsx_like: true,
                    })
                } else {
                    // export the file as a string
                    return Ok(ModuleData {
                        fm: fm.clone(),
                        module: swc_ecmascript::ast::Module {
                            span: Span::new(fm.start_pos, fm.end_pos, SyntaxContext::empty()),
                            body: vec![ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(
                                ExportDefaultExpr {
                                    span: Span::new(
                                        fm.start_pos,
                                        fm.end_pos,
                                        SyntaxContext::empty(),
                                    ),
                                    expr: Box::new(Expr::Lit(Lit::Str(Str {
                                        span: Span::new(
                                            fm.start_pos,
                                            fm.end_pos,
                                            SyntaxContext::empty(),
                                        ),
                                        value: JsWord::from(fm.src.as_str()),
                                        raw: None,
                                    }))),
                                },
                            ))],
                            shebang: None,
                        },
                        helpers: Default::default(),
                    });
                };

                let module = parse_file_as_module(
                    &fm,
                    syntax,
                    swc_ecmascript::ast::EsVersion::EsNext,
                    None,
                    &mut vec![],
                );

                self.progress.remove(&progress);

                let module = match module {
                    Ok(m) => m,
                    Err(e) => {
                        return Err(anyhow::Error::msg(
                            Error::new(e.span(), e.kind().msg(), "").display(&self.cm),
                        ))
                    }
                };

                /*
                module.body.insert(
                    0,
                    ModuleItem::ModuleDecl(
                        ModuleDecl::Import(ImportDecl {
                            span: Span::dummy_with_cmt(),
                            specifiers: vec![
                                ImportSpecifier::Named(
                                    ImportNamedSpecifier {
                                        span: Span::dummy_with_cmt(),
                                        local: Ident{
                                            span: Span::dummy_with_cmt(),
                                            sym: JsWord::from("Object"),
                                            optional: false
                                        },
                                        imported: None, is_type_only: false
                                    }
                                ),
                            ],
                            src: Box::new(Str{
                                span: Span::dummy_with_cmt(),
                                value: JsWord::from("std"),
                                raw: None
                            }),
                            type_only: false,
                            asserts: None
                        })
                    )
                );
                */

                return Ok(ModuleData {
                    fm: fm,
                    module,
                    helpers: Default::default(),
                });
            }
            FileName::Internal(name) => {
                if name == "std" {}
                todo!()
            }
            _ => Err(anyhow::anyhow!(
                "cannot import module '{}'.",
                file.to_string()
            )),
        }
    }
}

pub struct PathResolver {}

impl PathResolver {
    pub fn new() -> Self {
        Self {}
    }
}

impl swc_bundler::Resolve for PathResolver {
    fn resolve(&self, base: &FileName, module_specifier: &str) -> Result<FileName, anyhow::Error> {
        // extends current path parent
        if module_specifier.starts_with("./") {
            match base {
                FileName::Real(path) => {
                    let file = path.parent().unwrap().join(module_specifier);

                    if file.is_file() {
                        return Ok(FileName::Real(file));
                    }

                    let ts = file.with_extension("ts");
                    if ts.is_file() {
                        return Ok(FileName::Real(ts));
                    }

                    let js = file.with_extension("js");
                    if js.is_file() {
                        return Ok(FileName::Real(js));
                    }

                    return Err(anyhow::anyhow!(
                        "Cannot open file {}.",
                        js.as_path().as_os_str().to_string_lossy()
                    ));
                }
                FileName::Url(url) => return Ok(FileName::Url(url.join(module_specifier)?)),
                _ => todo!(),
            }
        } else {
            if module_specifier == "std" {
                return Ok(FileName::Internal("std".to_owned()));
            }

            todo!()
        }
    }
}

pub struct ModuleHook {}

impl ModuleHook {
    pub fn new() -> Self {
        Self {}
    }
}

impl swc_bundler::Hook for ModuleHook {
    fn get_import_meta_props(
        &self,
        span: swc_common::Span,
        module_record: &swc_bundler::ModuleRecord,
    ) -> Result<Vec<swc_ecmascript::ast::KeyValueProp>, anyhow::Error> {
        let url = match &module_record.file_name {
            FileName::Real(r) => {
                let full = r.canonicalize()?;
                full.to_string_lossy().to_string()
            }
            FileName::Url(u) => u.as_str().into(),
            _ => unimplemented!(),
        };
        return Ok(vec![KeyValueProp {
            key: PropName::Ident(Ident {
                span,
                sym: JsWord::from("url"),
                optional: false,
            }),
            value: Box::new(Expr::Lit(Lit::Str(Str {
                span,
                value: JsWord::from(url),
                raw: None,
            }))),
        }]);
    }
}
