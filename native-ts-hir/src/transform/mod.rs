mod class;
mod context;
mod expr;
mod function;
mod module;
mod stmt;
mod types;

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use context::*;

use native_js_common::error::Error;
use swc_common::Span;
use swc_ecmascript::ast as swc;

use crate::{
    ast::{self, Expr, Function, FunctionParam, ModuleExport, Stmt, Type},
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId, VariableId},
    symbol_table::SymbolTable,
    PropName,
};

type Result<T> = std::result::Result<T, Error<Span>>;

/// span, ty, fulfills
struct TypeCheck {
    span: Span,
    ty: Type,
    fulfills: Type,
}

pub struct Transformer {
    /// pended type checks tha cannot be done during translation
    type_checks: Vec<TypeCheck>,
    /// contains scope and definitions
    context: Context,

    break_labels: HashSet<String>,
    continue_labels: HashSet<String>,

    /// the current this type
    this_ty: Type,
    return_ty: Type,
    /// the current super class
    super_class: Option<ClassId>,
    /// indicates if current context is constructor
    is_in_constructor: bool,
}

impl Transformer {
    pub fn new() -> Self {
        Self {
            type_checks: Vec::new(),
            context: Context::new(),
            break_labels: Default::default(),
            continue_labels: Default::default(),
            this_ty: Type::Any,
            return_ty: Type::Undefined,
            super_class: None,
            is_in_constructor: false,
        }
    }
    pub fn anonymous_name(&self) -> String {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        let mut buf = native_js_common::itoa::Buffer::new();

        return "anonymous".to_string() + buf.format(COUNT.fetch_add(1, Ordering::SeqCst));
    }

    pub fn transform_module(&mut self, module: swc::Module) -> Result<crate::ast::Module> {
        let mut export_default = ModuleExport::Undefined;
        let mut module_exports = HashMap::new();

        for i in &module.body {
            if let swc::ModuleItem::ModuleDecl(swc::ModuleDecl::ExportDefaultDecl(d)) = i {
                let re = match &d.decl {
                    swc::DefaultDecl::Class(c) => {
                        self.hoist_class(c.ident.as_ref().map(|id| id.sym.as_ref()), &c.class)
                    }
                    swc::DefaultDecl::Fn(f) => {
                        self.hoist_function(f.ident.as_ref().map(|id| id.sym.as_ref()), &f.function)
                    }
                    swc::DefaultDecl::TsInterfaceDecl(i) => {
                        self.hoist_interface(Some(&i.id.sym), &i)
                    }
                };

                if let Err(e) = re {
                    return Err(e);
                }
            }
        }

        // hoist
        self.hoist(module.body.iter().filter_map(|i| {
            if let Some(m) = i.as_module_decl() {
                match m {
                    swc::ModuleDecl::ExportDecl(d) => {
                        return Some(&d.decl);
                    }
                    _ => None,
                }
            } else {
                return i.as_stmt().and_then(|s| s.as_decl());
            }
        }))?;

        for item in &module.body {
            match item {
                swc::ModuleItem::ModuleDecl(d) => match d {
                    swc::ModuleDecl::ExportDefaultDecl(decl) => match &decl.decl {
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

                            export_default = ModuleExport::Class(id);
                        }
                        swc::DefaultDecl::Fn(f) => {
                            let id = f
                                .ident
                                .as_ref()
                                .map(|id| self.context.get_func_id(&id.sym))
                                .unwrap_or(FunctionId::new());
                            self.translate_function(id, None, &f.function)?;
                            self.context.func().stmts.push(Stmt::DeclareFunction(id));

                            export_default = ModuleExport::Function(id);
                        }
                        swc::DefaultDecl::TsInterfaceDecl(i) => {
                            let id = self.context.get_interface_id(&i.id.sym);
                            self.context.func().stmts.push(Stmt::DeclareInterface(id));

                            export_default = ModuleExport::Interface(id);
                        }
                    },
                    swc::ModuleDecl::ExportDefaultExpr(expr) => {
                        let varid = VariableId::new();
                        let (expr, ty) = self.translate_expr(&expr.expr, None)?;
                        self.context
                            .func()
                            .stmts
                            .push(Stmt::DeclareVar(varid, ty.clone()));
                        self.context.func().stmts.push(Stmt::Expr(Expr::VarAssign {
                            op: crate::ast::AssignOp::Assign,
                            variable: varid,
                            value: Box::new(expr),
                        }));

                        export_default = ModuleExport::Var(varid, ty);
                    }
                    swc::ModuleDecl::ExportDecl(decl) => {
                        self.translate_decl(&decl.decl)?;
                    }
                    swc::ModuleDecl::ExportNamed(n) => {
                        if let Some(src) = &n.src {
                            let module_id = self.find_module(&src.value);

                            for s in &n.specifiers {
                                match s {
                                    swc::ExportSpecifier::Namespace(n) => {
                                        let name = self.translate_module_export_name(&n.name);
                                        module_exports
                                            .insert(name, ModuleExport::NameSpace(module_id));
                                    }
                                    swc::ExportSpecifier::Default(d) => {
                                        let name = PropName::Ident(d.exported.sym.to_string());
                                        module_exports
                                            .insert(name, self.module_default_export(module_id));
                                    }
                                    swc::ExportSpecifier::Named(n) => {
                                        let origin_name =
                                            self.translate_module_export_name(&n.orig);
                                        let module_export =
                                            self.module_export(module_id, &origin_name);

                                        if module_export.is_none() {
                                            return Err(Error::syntax_error(
                                                n.span,
                                                format!(
                                                    "module '{}' has no export '{}'",
                                                    src.value, origin_name
                                                ),
                                            ));
                                        }
                                        let module_export = module_export.unwrap();

                                        let exported_name = if let Some(exported) = &n.exported {
                                            self.translate_module_export_name(exported)
                                        } else {
                                            origin_name
                                        };

                                        if n.is_type_only {
                                            match &module_export {
                                                ModuleExport::Var(_, _)
                                                | ModuleExport::NameSpace(_) => {
                                                    return Err(Error::syntax_error(
                                                        n.span,
                                                        "type only export can only export type",
                                                    ))
                                                }
                                                _ => {}
                                            }
                                        }

                                        module_exports.insert(exported_name, module_export);
                                    }
                                }
                            }
                        } else {
                            for s in &n.specifiers {
                                match s {
                                    swc::ExportSpecifier::Named(n) => {
                                        let origin_name = match &n.orig {
                                            swc::ModuleExportName::Ident(id) => id.sym.to_string(),
                                            swc::ModuleExportName::Str(_) => unimplemented!(),
                                        };

                                        let binding = if let Some(bind) =
                                            self.context.find(&origin_name)
                                        {
                                            bind
                                        } else {
                                            return Err(Error::syntax_error(
                                                n.span,
                                                format!("undefined identifier '{}'", origin_name),
                                            ));
                                        };

                                        let export = match binding {
                                            Binding::Class(c) => ModuleExport::Class(*c),
                                            Binding::GenericClass(_) => todo!("export generic"),
                                            Binding::Enum(e) => ModuleExport::Enum(*e),
                                            Binding::Function(f) => ModuleExport::Function(*f),
                                            Binding::GenericFunction(_) => todo!("export generic"),
                                            Binding::Generic(_) => unreachable!(),
                                            Binding::Interface(i) => ModuleExport::Interface(*i),
                                            Binding::GenericInterface(_) => todo!("export generic"),
                                            Binding::TypeAlias(id) => ModuleExport::Alias(*id),
                                            Binding::GenericTypeAlias(_) => todo!("export generic"),
                                            Binding::Using { .. } => {
                                                // TODO: export using
                                                return Err(Error::syntax_error(n.span, "export 'using' declare is not allowed"))
                                            },
                                            Binding::Var { id, ty, .. } => {
                                                ModuleExport::Var(*id, ty.clone())
                                            }
                                            Binding::NameSpace(n) => ModuleExport::NameSpace(*n),
                                        };

                                        let exported_name = if let Some(exported) = &n.exported {
                                            self.translate_module_export_name(exported)
                                        } else {
                                            PropName::Ident(origin_name)
                                        };

                                        module_exports.insert(exported_name, export);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }
                    _ => {}
                },
                swc::ModuleItem::Stmt(s) => {
                    // translate statement
                    self.translate_stmt(s, None)?;
                }
            }
        }

        // finish up type checks
        for check in &self.type_checks {
            self.type_check(check.span, &check.ty, &check.fulfills)?;
        }

        let main = self.context.end_function();

        return Ok(ast::Module {
            table: SymbolTable {
                external_functions: Default::default(),
                functions: core::mem::replace(&mut self.context.functions, Default::default()),
                classes: core::mem::replace(&mut self.context.classes, Default::default()),
                interfaces: core::mem::replace(&mut self.context.interfaces, Default::default()),
                enums: core::mem::replace(&mut self.context.enums, Default::default()),
            },
            main_function: main,
            default_export: export_default,
            exports: module_exports,
        });
    }

    pub fn hoist_stmts<'a, I: Iterator<Item = &'a swc::Stmt> + Clone>(
        &mut self,
        stmts: I,
    ) -> Result<()> {
        self.hoist(stmts.filter_map(|s| s.as_decl()))
    }

    pub fn hoist<'a, I: Iterator<Item = &'a swc::Decl> + Clone>(&mut self, stmts: I) -> Result<()> {
        // hoist the type names
        for decl in stmts.clone() {
            match decl {
                swc::Decl::Class(class) => {
                    self.hoist_class(Some(&class.ident.sym), &class.class)?;
                }
                swc::Decl::Fn(func) => {
                    self.hoist_function(Some(&func.ident.sym), &func.function)?;
                }
                swc::Decl::TsEnum(e) => {
                    self.hoist_enum(&e)?;
                }
                swc::Decl::TsInterface(iface) => {
                    self.hoist_interface(Some(&iface.id.sym), &iface)?;
                }
                swc::Decl::TsModule(_m) => {
                    todo!("module declare")
                }
                swc::Decl::TsTypeAlias(alias) => {
                    // translate later
                    self.hoist_alias(&alias.id.sym, alias)?;
                }
                swc::Decl::Var(_) => {}
                // do not hoist using
                swc::Decl::Using(_) => {}
            }
        }

        // translate type alias
        for decl in stmts.clone() {
            if let swc::Decl::TsTypeAlias(a) = decl {
                match self.context.find(&a.id.sym) {
                    Some(Binding::TypeAlias(id)) => {
                        debug_assert!(
                            a.type_params.is_none()
                                || a.type_params.as_ref().is_some_and(|p| p.params.is_empty())
                        );

                        let id = *id;

                        // translate the type
                        let ty = self.translate_type_alias(&a)?;

                        self.context.alias.insert(id, ty);
                    }
                    Some(Binding::GenericTypeAlias(_id)) => {
                        todo!("generic alias")
                    }
                    _ => unreachable!(),
                }
            }
        }

        // translate all the interfaces
        for decl in stmts.clone() {
            if let swc::Decl::TsInterface(iface) = decl {
                match self.context.find(&iface.id.sym) {
                    Some(Binding::Interface(id)) => {
                        // copy the id
                        let id = *id;
                        // translate the interface
                        let ty = self.translate_interface(&iface)?;

                        let slot = self.context.interfaces.insert(id, ty);

                        // there should be no interface declared
                        debug_assert!(slot.is_none());
                    }
                    Some(Binding::GenericInterface(_id)) => {
                        todo!("generic interfaces")
                    }
                    _ => unreachable!(),
                };
            }
        }

        // translate classes
        for decl in stmts.clone() {
            if let swc::Decl::Class(class) = decl {
                match self.context.find(&class.ident.sym) {
                    Some(Binding::Class(id)) => {
                        let id = *id;
                        let c = self.translate_class_ty(id, &class.class)?;
                        self.context.classes.insert(id, c);
                    }
                    Some(Binding::GenericClass(_id)) => {
                        todo!("generic classes")
                    }
                    _ => unreachable!(),
                };
            }
        }

        // finish translating type and normalise them
        self.normalise_types();

        // hoist variables
        for decl in stmts.clone() {
            if let swc::Decl::Var(v) = decl {
                self.hoist_vardecl(v)?;
            }
        }

        // translate functions
        for decl in stmts {
            if let swc::Decl::Fn(func) = decl {
                match self.context.find(&func.ident.sym) {
                    Some(Binding::Function(id)) => {
                        let id = *id;
                        self.translate_function(id, None, &func.function)?;
                    }
                    Some(Binding::GenericFunction(_id)) => {
                        todo!("generic function")
                    }
                    _ => unreachable!(),
                }
            }
        }

        return Ok(());
    }

    pub fn hoist_class(&mut self, name: Option<&str>, class: &swc::Class) -> Result<()> {
        let id = ClassId::new();
        if class.type_params.is_some() {
            if !self
                .context
                .declare(name.unwrap_or(""), Binding::GenericClass(id))
            {
                return Err(Error::syntax_error(class.span, "duplicated identifier"));
            }
        } else {
            if !self.context.declare(name.unwrap_or(""), Binding::Class(id)) {
                return Err(Error::syntax_error(class.span, "duplicated identifier"));
            }
        }
        return Ok(());
    }

    pub fn hoist_function(&mut self, name: Option<&str>, func: &swc::Function) -> Result<()> {
        let id = FunctionId::new();
        if func.type_params.is_some() {
            if !self
                .context
                .declare(name.unwrap_or(""), Binding::GenericFunction(id))
            {
                return Err(Error::syntax_error(func.span, "duplicated identifier"));
            }
        } else {
            if !self
                .context
                .declare(name.unwrap_or(""), Binding::Function(id))
            {
                return Err(Error::syntax_error(func.span, "duplicated identifier"));
            }

            let f = self.translate_function_ty(func)?;

            // insert the function type
            self.context.functions.insert(
                id,
                Function {
                    this_ty: f.this_ty,
                    params: f
                        .params
                        .into_iter()
                        .map(|ty| FunctionParam {
                            id: VariableId::new(),
                            ty: ty,
                        })
                        .collect(),
                    return_ty: f.return_ty,
                    variables: Default::default(),
                    captures: Vec::new(),
                    stmts: Vec::new(),
                },
            );
        }
        return Ok(());
    }

    pub fn hoist_interface(
        &mut self,
        name: Option<&str>,
        iface: &swc::TsInterfaceDecl,
    ) -> Result<()> {
        let id = InterfaceId::new();
        if iface.type_params.is_some() {
            if !self
                .context
                .declare(name.unwrap_or(""), Binding::GenericInterface(id))
            {
                return Err(Error::syntax_error(iface.span, "duplicated identifier"));
            }
        } else {
            if !self
                .context
                .declare(name.unwrap_or(""), Binding::Interface(id))
            {
                return Err(Error::syntax_error(iface.span, "duplicated identifier"));
            }
        }
        return Ok(());
    }

    pub fn hoist_enum(&mut self, e: &swc::TsEnumDecl) -> Result<()> {
        let id = EnumId::new();
        if !self.context.declare(&e.id.sym, Binding::Enum(id)) {
            return Err(Error::syntax_error(e.id.span, "duplicated identifier"));
        }

        let ty = self.translate_enum(e)?;

        let slot = self.context.enums.insert(id, ty);

        debug_assert!(slot.is_none());

        return Ok(());
    }

    pub fn hoist_alias(&mut self, name: &str, alias: &swc::TsTypeAliasDecl) -> Result<()> {
        if alias.type_params.is_some() {
            if !self
                .context
                .declare(name, Binding::GenericTypeAlias(AliasId::new()))
            {
                return Err(Error::syntax_error(alias.span, "duplicated identifier"));
            }
        } else {
            if !self
                .context
                .declare(name, Binding::TypeAlias(AliasId::new()))
            {
                return Err(Error::syntax_error(alias.span, "duplicated identifier"));
            }
        }
        return Ok(());
    }

    pub fn hoist_vardecl(&mut self, _decl: &swc::VarDecl) -> Result<()> {
        return Ok(());
    }

    pub fn normalise_types(&mut self) {
        // types does not backreference therefore this function is safe
        unsafe {
            for alias in (self as *mut Self)
                .as_mut()
                .unwrap_unchecked()
                .context
                .alias
                .values_mut()
            {
                self.normalise_type(alias);
            }

            for iface in (self as *mut Self)
                .as_mut()
                .unwrap_unchecked()
                .context
                .interfaces
                .values_mut()
            {
                for (_name, prop) in &mut iface.properties {
                    self.normalise_type(&mut prop.ty)
                }
            }

            for class in (self as *mut Self)
                .as_mut()
                .unwrap_unchecked()
                .context
                .classes
                .values_mut()
            {
                for (_name, prop) in &mut class.properties {
                    self.normalise_type(&mut prop.ty);
                }
            }
        }
    }

    pub fn normalise_type(&mut self, ty: &mut Type) {
        match ty {
            Type::Alias(id) => {
                let t = unsafe { (self as *mut Self).as_mut().unwrap_unchecked() }
                    .context
                    .alias
                    .get_mut(id)
                    .expect("invalid alias type");
                self.normalise_type(t);

                *ty = t.clone();
            }
            Type::Array(elem) => {
                self.normalise_type(elem);
            }
            Type::Enum(_) => {}
            Type::Function(func) => {
                self.normalise_type(&mut func.this_ty);
                self.normalise_type(&mut func.return_ty);
                for param in &mut func.params {
                    self.normalise_type(param);
                }
            }
            Type::Map(key, value) => {
                self.normalise_type(key);
                self.normalise_type(value);
            }
            Type::Promise(re) => {
                self.normalise_type(re);
            }
            Type::Tuple(tys) => {
                for ty in tys.iter_mut() {
                    self.normalise_type(ty);
                }
            }
            Type::Iterator(ty) => {
                self.normalise_type(ty);
            }
            Type::Union(u) => {
                for i in u.iter_mut() {
                    self.normalise_type(i);
                }

                let mut has_union = false;
                for i in u.iter() {
                    if let Type::Union(_) = i {
                        has_union = true;
                    }
                }

                if !has_union {
                    return;
                }

                let mut tys = Vec::new();
                for i in u.iter() {
                    if let Type::Union(v) = i {
                        for ty in v.iter() {
                            if !tys.contains(ty) {
                                tys.push(ty.clone())
                            }
                        }
                    } else {
                        if !tys.contains(i) {
                            tys.push(i.clone());
                        }
                    }
                }

                tys.sort();

                *ty = Type::Union(tys.into_boxed_slice());
            }
            Type::Any
            | Type::AnyObject
            | Type::Bigint
            | Type::Bool
            | Type::Int
            | Type::Null
            | Type::Number
            | Type::Object(_)
            | Type::Regex
            | Type::String
            | Type::Symbol
            | Type::Undefined
            | Type::Interface(_)
            | Type::Generic(_) => {}
        }
    }
}
