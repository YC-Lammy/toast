mod class;
mod context;
mod expr;
mod module;
mod types;
mod function;

use std::sync::atomic::{AtomicUsize, Ordering};

use context::*;

use native_js_common::error::Error;
use swc_common::Span;
use swc_ecmascript::ast as swc;

use crate::{
    ast::Type,
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId},
};

type Result<T> = std::result::Result<T, Error<Span>>;

pub enum TypeCheck {
    Implements { span: Span, ty: Type, iface: Type },
    Extends { span: Span, ty: Type, extends: Type },
}

pub struct Transformer {
    /// pended type checks tha cannot be done during translation
    type_checks: Vec<TypeCheck>,
    /// contains scope and definitions
    context: Context,

    /// the current this type
    this_ty: Type,
    /// the current super class
    super_class: Option<ClassId>,
    /// indicates if current context is constructor
    is_in_constructor: bool
}

impl Transformer {
    pub fn anonymous_name(&self) -> String {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        let mut buf = native_js_common::itoa::Buffer::new();

        return "anonymous".to_string() + buf.format(COUNT.fetch_add(1, Ordering::SeqCst));
    }

    pub fn transform_module(&mut self, module: swc::Module) -> Result<crate::ast::Module> {
        for i in &module.body {
            if let swc::ModuleItem::ModuleDecl(swc::ModuleDecl::ExportDefaultDecl(d)) = i {
                let re = match &d.decl {
                    swc::DefaultDecl::Class(c) => self.hoist_class(None, &c.class),
                    swc::DefaultDecl::Fn(f) => self.hoist_function(None, &f.function),
                    swc::DefaultDecl::TsInterfaceDecl(i) => self.hoist_interface(None, &i),
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

        todo!()
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
                swc::Decl::Var(_) => {

                }
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
                        let c = self.translate_class_ty(&class.class)?;
                        self.context.classes.insert(id, c);
                    }
                    Some(Binding::GenericClass(_id)) => {
                        todo!("generic classes")
                    }
                    _ => unreachable!(),
                };
            }
        };

        // finish translating type and normalise them
        self.normalise_types();

        // hoist variables
        for decl in stmts.clone(){
            if let swc::Decl::Var(v) = decl{
                self.hoist_vardecl(v)?;
            }
        }

        // translate functions
        for decl in stmts{
            if let swc::Decl::Fn(func) = decl{
                match self.context.find(&func.ident.sym){
                    Some(Binding::Function(id)) => {
                        let id = *id;
                        self.translate_function(id, &func.function)?;
                    }
                    Some(Binding::GenericFunction(_id)) => {
                        todo!("generic function")
                    }
                    _ => unreachable!()
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

    pub fn hoist_vardecl(&mut self, decl: &swc::VarDecl) -> Result<()>{
        return Ok(())
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
