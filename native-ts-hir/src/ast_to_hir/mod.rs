
mod context;
mod expr;
mod hoist;
mod module;
mod properties;
mod stmt;
mod types;
//mod generic;

use std::{
    collections::{HashMap, HashSet},
    sync::{atomic::{AtomicUsize, Ordering}, Arc}
};

use context::*;

use native_ts_parser::swc_core::ecma::ast as swc;
use native_ts_parser::{swc_core::common::Span, ParsedProgram};
use parking_lot::RwLock;

use crate::{
    common::{ClassId, ModuleId},
    hir::{self, Program, Type},
    PropName,
};
use crate::{
    error::Error,
    hir::{ModuleTypeExport, ModuleValueExport},
};

type Result<T> = std::result::Result<T, Error>;

/// span, ty, fulfills
struct TypeCheck {
    span: Span,
    ty: Type,
    fulfills: Type,
}

pub struct Transformer {
    parsed_modules: Arc<RwLock<HashMap<ModuleId, crate::hir::Module>>>,

    export_default_value: Option<ModuleValueExport<Type>>,
    export_default_type: Option<ModuleTypeExport>,
    export_default_namespace: Option<ModuleId>,

    export_values: HashMap<PropName, ModuleValueExport<Type>>,
    export_types: HashMap<PropName, ModuleTypeExport>,
    export_namespace: HashMap<PropName, ModuleId>,

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
    pub fn new(parsed_modules: Arc<RwLock<HashMap<ModuleId, hir::Module>>>) -> Self {
        Self {
            parsed_modules: parsed_modules,

            export_default_namespace: None,
            export_default_type: None,
            export_default_value: None,

            export_values: HashMap::new(),
            export_types: HashMap::new(),
            export_namespace: HashMap::new(),

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
    pub(crate) fn anonymous_name(&self) -> String {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        let mut buf = itoa::Buffer::new();

        return "anonymous".to_string() + buf.format(COUNT.fetch_add(1, Ordering::SeqCst));
    }

    pub fn transform_program(mut self, program: &ParsedProgram) -> Result<crate::hir::Program> {
        for (id, m) in &program.modules {
            self.transform_module_with_dependencies(
                &program.modules,
                unsafe { core::mem::transmute(*id) },
                &m.module,
                unsafe { core::mem::transmute(m.dependencies.to_vec()) },
            )?;
        }

        return Ok(Program {
            table: self.context.symbol_table,
            entry: unsafe { core::mem::transmute(program.entry) },
            modules: self.parsed_modules,
        });
    }

    fn transform_module_with_dependencies(
        &mut self,
        modules: &HashMap<native_ts_parser::ModuleId, native_ts_parser::ParsedModule>,
        id: ModuleId,
        module: &swc::Module,
        dependencies: Vec<ModuleId>,
    ) -> Result<()> {
        // module already parsed
        if self.parsed_modules.read().contains_key(&id) {
            return Ok(());
        }

        for dep in &dependencies {
            if !self.parsed_modules.read().contains_key(dep) {
                let m = modules
                    .get(unsafe { &*(dep as *const _ as *const native_ts_parser::ModuleId) })
                    .expect("invalid module");
                self.transform_module_with_dependencies(modules, *dep, &m.module, unsafe {
                    core::mem::transmute(m.dependencies.to_vec())
                })?;
            }
        }

        // translate the module
        let mut parsed = self.transform_module(module)?;
        // write dependencies
        parsed.dependencies = dependencies;
        // insert module to registry
        assert!(
            self.parsed_modules.write().insert(id, parsed).is_none(),
            "module already parsed"
        );

        return Ok(());
    }

    fn transform_module(&mut self, module: &swc::Module) -> Result<crate::hir::Module> {
        // loop through declarations and hoist
        for i in &module.body {
            if let swc::ModuleItem::ModuleDecl(swc::ModuleDecl::ExportDefaultDecl(d)) = i {
                match &d.decl {
                    swc::DefaultDecl::Class(c) => {
                        self.hoist_class(c.ident.as_ref().map(|id| id.sym.as_ref()), &c.class)?;
                    }
                    swc::DefaultDecl::Fn(f) => {
                        self.hoist_function(
                            f.ident.as_ref().map(|id| id.sym.as_ref()),
                            &f.function,
                        )?;
                    }
                    swc::DefaultDecl::TsInterfaceDecl(i) => {
                        self.hoist_interface(Some(&i.id.sym), &i)?;
                    }
                };
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

        // translate module body
        for item in &module.body {
            match item {
                // translate import and export
                swc::ModuleItem::ModuleDecl(d) => match d {
                    // export default declaration
                    swc::ModuleDecl::ExportDefaultDecl(decl) => {
                        // export default decl
                        self.translate_export_default_decl(decl)?;
                    }
                    // export default expression
                    swc::ModuleDecl::ExportDefaultExpr(expr) => {
                        // export
                        self.translate_export_default_expr(&expr.expr)?;
                    }
                    // export declaration
                    swc::ModuleDecl::ExportDecl(decl) => {
                        self.translate_export_decl(&decl)?;
                    }
                    swc::ModuleDecl::ExportNamed(n) => {
                        self.translate_export_named(n)?;
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

        return Ok(hir::Module {
            main_function: main,

            default_value_export: self.export_default_value.clone(),
            default_type_export: self.export_default_type.clone(),
            default_namespace_export: self.export_default_namespace,

            value_exports: self.export_values.clone(),
            type_exports: self.export_types.clone(),
            namespcae_exports: self.export_namespace.clone(),

            dependencies: Vec::new(),
        });
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
            Type::NamespaceObject(_) => {}
            Type::LiteralObject(obj) => {
                for p in obj.iter() {
                    unsafe {
                        let p = (&p.ty as *const Type as *mut Type).as_mut().unwrap();
                        self.normalise_type(p);
                    }
                }
            }
            Type::Array(elem) => unsafe {
                let elem = (elem.as_ref() as *const Type as *mut Type)
                    .as_mut()
                    .unwrap();
                self.normalise_type(elem);
            },
            Type::Enum(_) => {}
            Type::Function(func) => unsafe {
                let this = (&func.this_ty as *const Type as *mut Type)
                    .as_mut()
                    .unwrap();
                self.normalise_type(this);

                let return_ty = (&func.return_ty as *const Type as *mut Type)
                    .as_mut()
                    .unwrap();
                self.normalise_type(return_ty);

                for param in &func.params {
                    let param = (param as *const Type as *mut Type).as_mut().unwrap();
                    self.normalise_type(param);
                }
            },
            Type::Map(key, value) => {
                self.normalise_type(key);
                self.normalise_type(value);
            }
            Type::Promise(re) => {
                self.normalise_type(re);
            }
            Type::Tuple(tys) => {
                for ty in tys.iter() {
                    unsafe {
                        let ty = (ty as *const Type as *mut Type).as_mut().unwrap();
                        self.normalise_type(ty);
                    }
                }
            }
            Type::Iterator(ty) => unsafe {
                let ty = (ty.as_ref() as *const Type as *mut Type).as_mut().unwrap();
                self.normalise_type(ty);
            },
            Type::Union(u) => {
                for i in u.iter() {
                    unsafe {
                        let i = (i as *const Type as *mut Type).as_mut().unwrap();
                        self.normalise_type(i);
                    }
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

                *ty = Type::Union(tys.into());
            }
            Type::Any
            | Type::AnyObject
            | Type::Bigint
            | Type::LiteralBigint(_)
            | Type::Bool
            | Type::LiteralBool(_)
            | Type::Int
            | Type::Null
            | Type::Number
            | Type::LiteralNumber(_)
            | Type::LiteralInt(_)
            | Type::Object(_)
            | Type::Regex
            | Type::String
            | Type::LiteralString(_)
            | Type::Symbol
            | Type::Undefined
            | Type::Interface(_) => {}
        }
    }
}
