use native_ts_parser::swc_core::common::Span;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::common::ModuleId;
use crate::common::{ClassId, FunctionId, VariableId};
use crate::error::Error;
use crate::hir::{Expr, ModuleTypeExport, ModuleValueExport, Stmt, Type, VarKind};
use crate::PropName;

use super::Transformer;
use super::{ClassBinding, TypeBinding, ValueBinding};

type Result<T> = core::result::Result<T, Error>;

impl Transformer {
    pub(super) fn find_module(&mut self, name: &str) -> ModuleId {
        todo!()
    }

    pub(super) fn module_default_export(
        &self,
        module: ModuleId,
    ) -> (
        Option<ModuleValueExport<Type>>,
        Option<ModuleTypeExport>,
        Option<ModuleId>,
    ) {
        let guard = self.parsed_modules.read();
        let m = guard.get(&module).expect("module not yet parsed");

        return (
            m.default_value_export.clone(),
            m.default_type_export.clone(),
            m.default_namespace_export.clone(),
        );
    }

    pub(super) fn module_export(
        &self,
        module: ModuleId,
        name: &PropName,
    ) -> (
        Option<ModuleValueExport<Type>>,
        Option<ModuleTypeExport>,
        Option<ModuleId>,
    ) {
        let guard = self.parsed_modules.read();

        let m = guard.get(&module).expect("module not yet parsed");

        let value = m.value_exports.get(name).cloned();
        let ty = m.type_exports.get(name).cloned();
        let namespcae = m.namespcae_exports.get(name).cloned();

        return (value, ty, namespcae);
    }

    pub(super) fn translate_module_export_name(
        &mut self,
        name: &swc::ModuleExportName,
    ) -> PropName {
        match name {
            swc::ModuleExportName::Ident(id) => PropName::Ident(id.sym.to_string()),
            swc::ModuleExportName::Str(s) => PropName::String(s.value.to_string()),
        }
    }

    pub(super) fn translate_export_default_decl(
        &mut self,
        decl: &swc::ExportDefaultDecl,
    ) -> Result<()> {
        match &decl.decl {
            swc::DefaultDecl::Class(c) => {
                let id = c
                    .ident
                    .as_ref()
                    .map(|id| self.context.get_class_id(&id.sym))
                    .unwrap_or(ClassId::new());

                self.translate_class(
                    id,
                    c.ident
                        .as_ref()
                        .map(|i| i.sym.as_ref())
                        .unwrap_or("default")
                        .to_string(),
                    &c.class,
                )?;

                self.context.func().stmts.push(Stmt::DeclareClass(id));

                // set default class export
                self.export_default_type = Some(ModuleTypeExport::Class(id));
            }
            swc::DefaultDecl::Fn(f) => {
                let id = f
                    .ident
                    .as_ref()
                    .map(|id| self.context.get_func_id(&id.sym))
                    .unwrap_or(FunctionId::new());
                self.translate_function(id, None, &f.function)?;
                self.context.func().stmts.push(Stmt::DeclareFunction(id));

                // set default value export
                self.export_default_value = Some(ModuleValueExport::Function(id));
            }
            swc::DefaultDecl::TsInterfaceDecl(i) => {
                let id = self.context.get_interface_id(&i.id.sym);
                self.context.func().stmts.push(Stmt::DeclareInterface(id));

                // set default type export
                self.export_default_type = Some(ModuleTypeExport::Interface(id));
            }
        }

        return Ok(());
    }

    pub(super) fn translate_export_default_expr(
        &mut self,
        span: Span,
        expr: &swc::Expr,
    ) -> Result<()> {
        // create new variable
        let varid = VariableId::new();

        // translate expression
        let (expr, ty) = self.translate_expr(&expr, None)?;

        self.context.declare_global(varid, ty.clone());

        // declare variable
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Let,
            id: varid,
            ty: ty.clone(),
        });
        // assign expression
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                span: span,
                op: crate::hir::AssignOp::Assign,
                variable: varid,
                value: Box::new(expr),
            })));

        // set export default value
        self.export_default_value = Some(ModuleValueExport::Var(varid, ty));

        return Ok(());
    }

    pub(super) fn translate_export_named(&mut self, export_named: &swc::NamedExport) -> Result<()> {
        // re-export named exports
        if let Some(src) = &export_named.src {
            return self.translate_export_named_from_src(&export_named.specifiers, &src);
        }

        // loop through specifiers
        for s in &export_named.specifiers {
            match s {
                swc::ExportSpecifier::Named(n) => {
                    // the original name of the thing
                    let origin_name = match &n.orig {
                        swc::ModuleExportName::Ident(id) => id.sym.as_str(),
                        swc::ModuleExportName::Str(_) => unimplemented!(),
                    };

                    // exported name
                    let exported_name = if let Some(exported) = &n.exported {
                        self.translate_module_export_name(exported)
                    } else {
                        match &n.orig {
                            swc::ModuleExportName::Ident(id) => PropName::Ident(id.sym.to_string()),
                            swc::ModuleExportName::Str(s) => PropName::String(s.value.to_string()),
                        }
                    };

                    // no binding is found
                    if !self.context.has_binding(&origin_name) {
                        return Err(Error::syntax_error(
                            n.span,
                            format!("undefined identifier '{}'", origin_name),
                        ));
                    }

                    // has class binding
                    match self.context.find_class_binding(&origin_name) {
                        // export class as type
                        Some(ClassBinding::Class(id)) => {
                            self.export_types
                                .insert(exported_name.clone(), ModuleTypeExport::Class(*id));
                        }
                        Some(ClassBinding::Generic(id)) => {
                            // export class as type
                            self.export_types
                                .insert(exported_name.clone(), ModuleTypeExport::GenericClass(*id));
                        }
                        None => {}
                    }

                    // has type binding
                    match self.context.find_type_binding(&origin_name) {
                        Some(TypeBinding::Enum(id)) => {
                            // export enum as type
                            self.export_types
                                .insert(exported_name.clone(), ModuleTypeExport::Enum(*id));
                        }
                        Some(TypeBinding::Interface(id)) => {
                            // export interface as type
                            self.export_types
                                .insert(exported_name.clone(), ModuleTypeExport::Interface(*id));
                        }
                        Some(TypeBinding::GenericInterface(id)) => {
                            // export generic interface as type
                            self.export_types.insert(
                                exported_name.clone(),
                                ModuleTypeExport::GenericInterface(*id),
                            );
                        }
                        Some(TypeBinding::TypeAlias(id)) => {
                            // export alias as type
                            self.export_types
                                .insert(exported_name.clone(), ModuleTypeExport::TypeAlias(*id));
                        }
                        Some(TypeBinding::GenericTypeAlias(id)) => {
                            // export generic alias as type
                            self.export_types.insert(
                                exported_name.clone(),
                                ModuleTypeExport::GenericTypeAlias(*id),
                            );
                        }
                        // generic type should not appear in module scope
                        Some(TypeBinding::Generic(_)) => unreachable!(),
                        None => {}
                    }

                    // has value binding
                    match self.context.find_value_binding(&origin_name) {
                        Some(ValueBinding::Function(id)) => {
                            self.export_values
                                .insert(exported_name.clone(), ModuleValueExport::Function(*id));
                        }
                        Some(ValueBinding::GenericFunction(id)) => {
                            self.export_values.insert(
                                exported_name.clone(),
                                ModuleValueExport::GenericFunction(*id),
                            );
                        }
                        Some(ValueBinding::Var { id, ty, .. }) => {
                            self.export_values.insert(
                                exported_name.clone(),
                                ModuleValueExport::Var(*id, ty.clone()),
                            );
                        }
                        Some(ValueBinding::Using { .. }) => {
                            return Err(Error::syntax_error(
                                n.span,
                                "'export' modifier cannot appear on a 'using' declaration.",
                            ))
                        }
                        None => {}
                    }

                    // has namespace binding
                    match self.context.find_namespace_binding(&origin_name) {
                        Some(id) => {
                            self.export_namespace.insert(exported_name, id);
                        }
                        None => {}
                    }
                }
                _ => unreachable!(),
            }
        }

        return Ok(());
    }

    fn translate_export_named_from_src(
        &mut self,
        specifiers: &[swc::ExportSpecifier],
        src: &swc::Str,
    ) -> Result<()> {
        // module id
        let module_id = self.find_module(&src.value);

        for s in specifiers {
            match s {
                swc::ExportSpecifier::Namespace(n) => {
                    // exported name
                    let name = self.translate_module_export_name(&n.name);

                    // export namespace
                    self.export_namespace.insert(name, module_id);
                }
                swc::ExportSpecifier::Default(d) => {
                    // exported name
                    let name = PropName::Ident(d.exported.sym.to_string());

                    let (value, ty, namespace) = self.module_default_export(module_id);

                    if let Some(value) = value {
                        // re-export value
                        self.export_values.insert(name.clone(), value);
                    }

                    if let Some(ty) = ty {
                        // re-export type
                        self.export_types.insert(name.clone(), ty);
                    }

                    if let Some(namespace) = namespace {
                        // re-export namespace
                        self.export_namespace.insert(name, namespace);
                    }
                }
                swc::ExportSpecifier::Named(n) => {
                    // original name
                    let origin_name = self.translate_module_export_name(&n.orig);

                    // get the exports by name
                    let (value, ty, namespace) = self.module_export(module_id, &origin_name);

                    if value.is_none() && ty.is_none() && namespace.is_none() {
                        return Err(Error::syntax_error(
                            n.span,
                            format!("module '{}' has no export '{}'", src.value, origin_name),
                        ));
                    }

                    // exported name
                    let exported_name = if let Some(exported) = &n.exported {
                        self.translate_module_export_name(exported)
                    } else {
                        origin_name
                    };

                    if let Some(value) = value {
                        // re-export value
                        self.export_values.insert(exported_name.clone(), value);
                    }

                    if let Some(ty) = ty {
                        // re-export type
                        self.export_types.insert(exported_name.clone(), ty);
                    }

                    if let Some(namespace) = namespace {
                        // re-export namespace
                        self.export_namespace.insert(exported_name, namespace);
                    }
                }
            }
        }

        return Ok(());
    }

    pub(super) fn translate_export_decl(&mut self, decl: &swc::ExportDecl) -> Result<()> {
        // translate the declare body
        let ids = self.translate_decl(&decl.decl)?;

        match &decl.decl {
            swc::Decl::Class(c) => {
                match self.context.find_class_binding(&c.ident.sym) {
                    Some(ClassBinding::Class(id)) => {
                        // export class
                        self.export_types.insert(
                            PropName::Ident(c.ident.sym.to_string()),
                            ModuleTypeExport::Class(*id),
                        );
                    }
                    Some(ClassBinding::Generic(id)) => {
                        // export generic class
                        self.export_types.insert(
                            PropName::Ident(c.ident.sym.to_string()),
                            ModuleTypeExport::GenericClass(*id),
                        );
                    }
                    // should be declared
                    None => unreachable!(),
                }
            }
            swc::Decl::Fn(f) => {
                match self.context.find_value_binding(&f.ident.sym) {
                    Some(ValueBinding::Function(id)) => {
                        // export function
                        self.export_values.insert(
                            PropName::Ident(f.ident.sym.to_string()),
                            ModuleValueExport::Function(*id),
                        );
                    }
                    Some(ValueBinding::GenericFunction(id)) => {
                        // export generic function
                        self.export_values.insert(
                            PropName::Ident(f.ident.sym.to_string()),
                            ModuleValueExport::GenericFunction(*id),
                        );
                    }
                    _ => unreachable!(),
                }
            }
            swc::Decl::Var(_v) => {
                let ids = ids.expect("expected variable declare");

                for id in ids {
                    // get name and type for variable
                    let (name, ty) = self
                        .context
                        .get_variable_name_ty(id)
                        .expect("variable name");

                    // export variable
                    self.export_values.insert(
                        PropName::Ident(name.to_string()),
                        ModuleValueExport::Var(id, ty.clone()),
                    );
                }
            }
            swc::Decl::Using(u) => {
                return Err(Error::syntax_error(
                    u.span,
                    "'export' modifier cannot appear on a 'using' declaration.",
                ))
            }
            swc::Decl::TsEnum(e) => {
                match self.context.find_type_binding(&e.id.sym) {
                    // export enum type
                    Some(TypeBinding::Enum(id)) => {
                        self.export_types.insert(
                            PropName::Ident(e.id.sym.to_string()),
                            ModuleTypeExport::Enum(*id),
                        );
                    }
                    _ => unreachable!(),
                }
            }
            swc::Decl::TsInterface(i) => {
                match self.context.find_type_binding(&i.id.sym) {
                    Some(TypeBinding::Interface(id)) => {
                        // export interface type
                        self.export_types.insert(
                            PropName::Ident(i.id.sym.to_string()),
                            ModuleTypeExport::Interface(*id),
                        );
                    }
                    Some(TypeBinding::GenericInterface(id)) => {
                        // export generic interface
                        self.export_types.insert(
                            PropName::Ident(i.id.sym.to_string()),
                            ModuleTypeExport::GenericInterface(*id),
                        );
                    }
                    _ => unreachable!(),
                }
            }
            swc::Decl::TsTypeAlias(a) => {
                match self.context.find_type_binding(&a.id.sym) {
                    Some(TypeBinding::TypeAlias(id)) => {
                        // export type alias
                        self.export_types.insert(
                            PropName::Ident(a.id.sym.to_string()),
                            ModuleTypeExport::TypeAlias(*id),
                        );
                    }
                    Some(TypeBinding::GenericTypeAlias(id)) => {
                        // export generic type alias
                        self.export_types.insert(
                            PropName::Ident(a.id.sym.to_string()),
                            ModuleTypeExport::GenericTypeAlias(*id),
                        );
                    }
                    _ => unreachable!(),
                }
            }
            swc::Decl::TsModule(m) => {
                let name = self.translate_module_name(&m.id);

                match self.context.find_namespace_binding(&name) {
                    Some(id) => {
                        self.export_namespace.insert(
                            match &m.id {
                                swc::TsModuleName::Ident(id) => PropName::Ident(id.sym.to_string()),
                                swc::TsModuleName::Str(s) => PropName::String(s.value.to_string()),
                            },
                            id,
                        );
                    }
                    None => unreachable!(),
                }
            }
        };

        return Ok(());
    }

    fn translate_module_name(&self, name: &swc::TsModuleName) -> String {
        todo!()
    }
}
