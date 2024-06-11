use native_ts_parser::swc_core::ecma::ast as swc;

use crate::common::{GenericAliasId, GenericInterfaceId};
use crate::error::Error;
use crate::util::OR;
use crate::{
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId, VariableId},
    hir::{Function, FunctionParam},
};

use super::{ClassBinding, Transformer, TypeBinding};

type Result<T> = std::result::Result<T, Error>;

impl Transformer {
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
                // hoist later
                swc::Decl::Fn(_) => {}
                swc::Decl::Var(_) => {}
                // do not hoist using
                swc::Decl::Using(_) => {}
            }
        }

        // translate type alias
        for decl in stmts.clone() {
            if let swc::Decl::TsTypeAlias(a) = decl {
                match self.context.find_type_binding(&a.id.sym) {
                    Some(TypeBinding::TypeAlias(id)) => {
                        // should not have type argments
                        debug_assert!(
                            a.type_params.is_none()
                                || a.type_params.as_ref().is_some_and(|p| p.params.is_empty())
                        );

                        // copy the id to avoid borrow
                        let id = *id;

                        // translate the type
                        let ty = self.translate_type_alias(&a)?;

                        // bind alias to context
                        self.context.alias.insert(id, ty);
                    }
                    Some(TypeBinding::GenericTypeAlias(id)) => {
                        todo!("generic alias")
                    }
                    _ => unreachable!(),
                }
            }
        }

        // translate all the interfaces
        for decl in stmts.clone() {
            if let swc::Decl::TsInterface(iface) = decl {
                match self.context.find_type_binding(&iface.id.sym) {
                    Some(TypeBinding::Interface(id)) => {
                        // copy the id
                        let id = *id;
                        // translate the interface
                        let ty = self.translate_interface(&iface)?;

                        let slot = self.context.interfaces.insert(id, ty);

                        // there should be no interface declared
                        debug_assert!(slot.is_none());
                    }
                    Some(TypeBinding::GenericInterface(id)) => {
                        todo!("generic interfaces")
                    }
                    _ => unreachable!(),
                };
            }
        }

        // translate classes
        for decl in stmts.clone() {
            if let swc::Decl::Class(class) = decl {
                match self.context.find_class_binding(&class.ident.sym) {
                    Some(ClassBinding::Class(id)) => {
                        let id = *id;
                        let c = self.translate_class_ty(id, &class.class)?;
                        self.context.classes.insert(id, c);
                    }
                    Some(ClassBinding::Generic(id)) => {
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

        // hoist functions
        for decl in stmts {
            if let swc::Decl::Fn(f) = decl {
                self.hoist_function(Some(&f.ident.sym), &f.function)?;
            }
        }

        return Ok(());
    }

    pub fn hoist_class(&mut self, name: Option<&str>, class: &swc::Class) -> Result<()> {
        // create a new class id
        let id = ClassId::new();

        let name = name.unwrap_or("");

        // check for generic paramaters
        if class.type_params.is_some() {
            // declare generic class
            if !self.context.bind_generic_class(name, id) {
                // identifier is already used
                return Err(Error::syntax_error(class.span, "duplicated identifier"));
            }
        } else {
            // declare the class
            if !self.context.bind_class(name, id) {
                // identifier is already used
                return Err(Error::syntax_error(class.span, "duplicated identifier"));
            }
        }
        return Ok(());
    }

    pub fn hoist_function(
        &mut self,
        name: Option<&str>,
        func: &swc::Function,
    ) -> Result<FunctionId> {
        let id = FunctionId::new();
        if func.type_params.is_some() {
            if let Some(name) = name {
                if !self.context.bind_generic_function(name, id) {
                    return Err(Error::syntax_error(func.span, "duplicated identifier"));
                }
            }
        } else {
            if let Some(name) = name {
                if !self.context.bind_function(name, id) {
                    return Err(Error::syntax_error(func.span, "duplicated identifier"));
                }
            }

            let f = self.translate_function_ty(func)?;

            // insert the function type
            self.context.functions.insert(
                id,
                Function {
                    span: func.span,
                    is_async: func.is_async,
                    is_generator: func.is_generator,
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
        return Ok(id);
    }

    pub fn hoist_interface(
        &mut self,
        name: Option<&str>,
        iface: &swc::TsInterfaceDecl,
    ) -> Result<OR<InterfaceId, GenericInterfaceId>> {
        let name = name.unwrap_or("");
        // a generic interface
        if iface.type_params.is_some() {
            // create new id for interface
            let mut id = GenericInterfaceId::new();

            // declare generic interface
            if !self.context.bind_generic_interface(name, id) {
                // identifier already used
                return Err(Error::syntax_error(iface.span, "duplicated identifier"));
            }

            return Ok(OR::B(id));
        } else {
            // create a new id for interface
            let mut id = InterfaceId::new();

            // declare interface
            match self.context.bind_interface(name, id) {
                Ok(Some(v)) => id = v,
                Ok(None) => {}
                // identifier already used
                Err(_) => return Err(Error::syntax_error(iface.span, "duplicated identifier")),
            }

            return Ok(OR::A(id));
        };
    }

    pub fn hoist_enum(&mut self, e: &swc::TsEnumDecl) -> Result<()> {
        // create new id
        let id = EnumId::new();

        // declare enum
        if !self.context.bind_enum(&e.id.sym, id) {
            return Err(Error::syntax_error(e.id.span, "duplicated identifier"));
        }

        // translate enum already
        let ty = self.translate_enum(e)?;

        // insert enum to context
        let slot = self.context.enums.insert(id, ty);
        // should not have been declared
        debug_assert!(slot.is_none());

        return Ok(());
    }

    pub fn hoist_alias(&mut self, name: &str, alias: &swc::TsTypeAliasDecl) -> Result<()> {
        // generic alias
        if alias.type_params.is_some() {
            let id = GenericAliasId::new();

            if !self.context.bind_generic_type_alias(name, id) {
                return Err(Error::syntax_error(alias.span, "duplicated identifier"));
            }
        } else {
            let id = AliasId::new();
            // not generic
            if !self.context.bind_type_alias(name, id) {
                return Err(Error::syntax_error(alias.span, "duplicated identifier"));
            }
        }
        return Ok(());
    }

    pub fn hoist_vardecl(&mut self, _decl: &swc::VarDecl) -> Result<()> {
        return Ok(());
    }
}
