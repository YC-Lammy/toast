use std::collections::HashMap;

use swc_bundler::{Bundle, BundleKind};
use swc_common::{sync::Lrc, FileName, Globals, Mark, SourceMap};
use swc_ecmascript::ast::*;
use swc_ecmascript::{ast::Module, visit::FoldWith};

use crate::{dependencies, Configuration};

#[derive(Debug, Default, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ModuleId(pub u64);

#[derive(Debug)]
pub struct ParsedPackage {
    pub config: Configuration,
    pub bundles: Vec<Bundle>,
    pub module: Module,
}

impl ParsedPackage {
    pub fn new(
        config: Configuration,
        main: &str,
        source_map: Lrc<SourceMap>,
    ) -> anyhow::Result<ParsedPackage> {
        //let mut recovered_errors = Vec::new();

        let globals = Globals::new();

        let mut bundler = swc_bundler::Bundler::new(
            &globals,
            source_map.clone(),
            dependencies::PathLoader::new(source_map.clone()),
            dependencies::PathResolver::new(),
            swc_bundler::Config {
                require: true,
                disable_inliner: false,
                disable_hygiene: false,
                disable_fixer: false,
                disable_dce: false,
                external_modules: vec![],
                module: swc_bundler::ModuleType::Es,
            },
            Box::new(dependencies::ModuleHook::new()),
        );

        let mut entries = HashMap::new();
        entries.insert("main".to_string(), FileName::Real(main.into()));

        let mut bundles = bundler.bundle(entries)?;

        println!("bundling finished, {} bundles generated.", bundles.len());

        let writer = swc_ecmascript::codegen::text_writer::JsWriter::new(
            source_map.clone(),
            "\n",
            std::io::stdout(),
            None,
        );
        let mut emitter = swc_ecmascript::codegen::Emitter {
            cfg: swc_ecmascript::codegen::Config {
                target: EsVersion::EsNext,
                ascii_only: false,
                minify: false,
                omit_last_semi: false,
            },
            cm: source_map.clone(),
            comments: None,
            wr: writer,
        };

        let mut i = 0;
        for bundle in &bundles {
            #[cfg(debug_assertions)]
            {
                println!("{:?}", bundle.kind);
                println!("id {}\n", bundle.id);
                emitter
                    .emit_module(&bundle.module)
                    .expect("fail to emit module");
                println!("\n");
            };

            if let BundleKind::Named { name } = &bundle.kind {
                if name == "main" {
                    break;
                }
            }
            i += 1;
        }

        let mut main = bundles.remove(i);

        // transform the bundle
        let main = swc_common::GLOBALS.set(&globals, || {
            //let unresolved_mark = Mark::new();
            let top_level_mark = Mark::new();

            //main.module = main.module.fold_with(&mut swc_ecmascript::transforms::resolver(unresolved_mark, top_level_mark, true));
            main.module =
                main.module
                    .fold_with(&mut swc_ecmascript::transforms::typescript::strip(
                        top_level_mark,
                    ));
            return main;
        });

        if config.emit_bundled_js {
            let file = std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(format!(
                    "compiler_output_bundled-{}.js",
                    std::time::SystemTime::now()
                        .duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .unwrap()
                        .as_millis()
                ))
                .expect("failed to open file for bundling.");

            let writer = swc_ecmascript::codegen::text_writer::JsWriter::new(
                source_map.clone(),
                "\n",
                file,
                None,
            );
            let mut emitter = swc_ecmascript::codegen::Emitter {
                cfg: swc_ecmascript::codegen::Config {
                    target: EsVersion::EsNext,
                    ascii_only: false,
                    minify: false,
                    omit_last_semi: false,
                },
                cm: source_map.clone(),
                comments: None,
                wr: writer,
            };

            emitter.emit_module(&main.module)?;
        }

        return Ok(ParsedPackage {
            config,
            bundles,
            module: main.module,
        });
    }
}
/*
        let current_path = std::env::current_dir()?;

        let main_id = dependencies::resolve_module(current_path.as_path(), main)?;

        let mut package = ParsedPackage {
            modules: Default::default(),
            main: main_id,
            config,
        };

        let main_src = dependencies::get_module_source(main_id)?;
        let main_module = swc_ecmascript::parser::parse_file_as_module(
            &main_src,
            syntax,
            EsVersion::Es2022,
            None,
            &mut recovered_errors,
        )
        .map_err(|e| format_swc_error(e, &main_src))?;

        let main_imports = resolve_imports(main_module, &current_path, main_id)?;
        let main_module = ParsedModule {
            id: main_id,
            local_imports: main_imports.0.clone(),
            external_imports: main_imports.1.clone(),
            stmts: main_imports.2,
        };

        package.modules.insert(main_id, main_module);

        for id in &main_imports.0 {
            parse_import(*id, &mut package, syntax, &mut recovered_errors)?;
        }

        for id in &main_imports.1 {
            parse_import(*id, &mut package, syntax, &mut recovered_errors)?;
        }

        return Ok(package);

    }

    /// find cyclic imports using DFS (Depth First Traverse)
    pub fn find_cyclic_import(&self) -> Vec<Vec<ModuleId>> {
        let mut cycles = Vec::new();
        let mut stack: VecDeque<ModuleId> = VecDeque::new();
        let mut seen: HashSet<ModuleId> = HashSet::new();

        for next in self.modules.keys() {
            if seen.contains(next) {
                continue;
            }

            let mut paths: HashMap<ModuleId, ModuleId> = HashMap::new();
            stack.push_back(*next);

            while let Some(current) = stack.pop_front() {
                seen.insert(current);

                for child in self
                    .modules
                    .get(&current)
                    .as_ref()
                    .unwrap()
                    .local_imports
                    .iter()
                {
                    if seen.contains(child) {
                        cycles.push(self.build_cycles(&mut paths, *child, current));
                    } else {
                        stack.push_back(*child);
                        paths.insert(*child, current);
                        seen.insert(*child);
                    }
                }
            }
        }

        return cycles;
    }

    /// build cycle graph from paths
    fn build_cycles(
        &self,
        paths: &mut HashMap<ModuleId, ModuleId>,
        start: ModuleId,
        end: ModuleId,
    ) -> Vec<ModuleId> {
        let mut cycles = VecDeque::new();
        let mut current = end;

        while current != start {
            cycles.push_front(current);
            current = if let Some(p) = paths.get(&current) {
                *p
            } else {
                break;
            }
        }

        cycles.push_front(start);

        return cycles.into();
    }
}

fn parse_import(
    id: ModuleId,
    package: &mut ParsedPackage,
    syntax: Syntax,
    recovered_errors: &mut Vec<swc_ecmascript::parser::error::Error>,
) -> anyhow::Result<()> {
    let file = dependencies::get_filepath_from_id(id);
    let current_path = file.as_path().parent().unwrap();

    let src = dependencies::get_module_source(id)?;

    let module = swc_ecmascript::parser::parse_file_as_module(
        &src,
        syntax,
        EsVersion::Es2022,
        None,
        recovered_errors,
    )
    .map_err(|e| format_swc_error(e, &src))?;

    let i = resolve_imports(module, current_path, id)?;

    package.modules.insert(
        id,
        ParsedModule {
            id: id,
            local_imports: i.0.clone(),
            external_imports: i.1.clone(),
            stmts: i.2,
        },
    );

    for mid in i.0 {
        if !package.modules.contains_key(&mid) {
            parse_import(mid, package, syntax, recovered_errors)?;
        }
    }

    for mid in i.1 {
        if !package.modules.contains_key(&mid) {
            parse_import(mid, package, syntax, recovered_errors)?;
        }
    }

    return Ok(());
}

fn resolve_imports(
    module: Module,
    current_path: &Path,
    module_id: ModuleId,
) -> anyhow::Result<(Vec<ModuleId>, Vec<ModuleId>, Vec<ImportOrStmt>)> {
    let mut local_imports = Vec::new();
    let mut external_imports = Vec::new();
    let mut statments = Vec::new();

    for i in module.body {
        match i {
            ModuleItem::Stmt(s) => statments.push(ImportOrStmt::Stmt(s)),
            ModuleItem::ModuleDecl(d) => {
                match d {
                    ModuleDecl::Import(d) => {
                        let module =
                            dependencies::resolve_module(current_path, d.src.value.as_ref())?;

                        if dependencies::is_external_module(d.src.value.as_ref()) {
                            external_imports.push(module);
                        } else {
                            {
                                local_imports.push(module);
                            }
                        };

                        if d.specifiers.len() == 0 {
                            statments.push(ImportOrStmt::Import(ImportStmt::ImportOnly {
                                id: module,
                                span: d.span,
                            }));
                        }

                        for i in d.specifiers {
                            match i {
                                ImportSpecifier::Default(d) => {
                                    statments.push(ImportOrStmt::Import(ImportStmt::Default {
                                        name: d.local.sym,
                                        module,
                                        span: d.span,
                                    }));
                                }
                                ImportSpecifier::Named(n) => {
                                    let name = if let Some(n) = &n.imported {
                                        match n {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        }
                                    } else {
                                        n.local.sym.clone()
                                    };

                                    let alias = if n.imported.is_some() {
                                        Some(n.local.sym.clone())
                                    } else {
                                        None
                                    };
                                    statments.push(ImportOrStmt::Import(ImportStmt::Named {
                                        name: name,
                                        alias,
                                        module,
                                        span: d.span,
                                    }));
                                }
                                ImportSpecifier::Namespace(n) => {
                                    statments.push(ImportOrStmt::Import(ImportStmt::AllAs {
                                        name: n.local.sym,
                                        module,
                                        span: d.span,
                                    }));
                                }
                            }
                        }
                    }
                    ModuleDecl::ExportAll(e) => {
                        let module =
                            dependencies::resolve_module(current_path, e.src.value.as_ref())?;
                        statments.push(ImportOrStmt::Export(ExportStmt::ExportAllFrom {
                            alias: None,
                            module,
                        }));
                    }
                    ModuleDecl::ExportNamed(e) => {
                        if let Some(n) = &e.src {
                            let module =
                                dependencies::resolve_module(current_path, n.value.as_ref())?;

                            for i in e.specifiers {
                                match i {
                                    ExportSpecifier::Default(d) => {
                                        statments.push(ImportOrStmt::Export(
                                            ExportStmt::ExportDefaultFrom {
                                                name: d.exported.sym,
                                                module,
                                                span: e.span,
                                            },
                                        ));
                                    }
                                    ExportSpecifier::Named(n) => {
                                        let name = match &n.orig {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        };

                                        let alias = n.exported.as_ref().map(|e| match e {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        });

                                        statments.push(ImportOrStmt::Export(
                                            ExportStmt::ExportNamedFrom {
                                                name: name,
                                                alias: alias,
                                                module,
                                                span: e.span,
                                            },
                                        ));
                                    }
                                    ExportSpecifier::Namespace(e) => {
                                        let alias = match &e.name {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        };

                                        statments.push(ImportOrStmt::Export(
                                            ExportStmt::ExportAllFrom {
                                                alias: Some(alias),
                                                module,
                                            },
                                        ));
                                    }
                                }
                            }
                        } else {
                            for i in e.specifiers {
                                match i {
                                    ExportSpecifier::Named(n) => {
                                        let name = match &n.orig {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        };

                                        let alias = n.exported.as_ref().map(|e| match e {
                                            ModuleExportName::Ident(i) => i.sym.clone(),
                                            ModuleExportName::Str(s) => s.value.clone(),
                                        });

                                        statments.push(ImportOrStmt::Export(
                                            ExportStmt::ExportNamed {
                                                name,
                                                alias,
                                                span: e.span,
                                            },
                                        ));
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        };
                    }
                    ModuleDecl::ExportDecl(d) => {
                        statments.push(ImportOrStmt::Export(ExportStmt::ExportDecl(d)))
                    }
                    ModuleDecl::ExportDefaultDecl(d) => {
                        statments.push(ImportOrStmt::Export(ExportStmt::ExportDefaultDecl(d)))
                    }
                    ModuleDecl::ExportDefaultExpr(d) => {
                        statments.push(ImportOrStmt::Export(ExportStmt::ExportDefaultExpr(d)))
                    }

                    // export as namespace ident;
                    ModuleDecl::TsNamespaceExport(_e) => {
                        let _ = module_id;
                        todo!("ts namespace export")
                    }

                    decl => todo!("{:?}", decl),
                }
            }
        };
    }

    return Ok((local_imports, external_imports, statments));
}

pub fn format_swc_error(
    e: swc_ecmascript::parser::error::Error,
    src: &SourceFile,
) -> anyhow::Error {
    anyhow::Error::msg(Error::with_source(src, e.span(), e.kind().msg(), ""))
}
*/
