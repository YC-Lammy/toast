
use std::collections::HashMap;

use native_ts_parser::swc_core::{common::{Span, Spanned}, ecma::ast as swc};
use num_traits::ToPrimitive;

use crate::{
    ast::{Function, FunctionParam, GenericFuncType, GenericParam, Program, Type, TypeOperator},
    common::{AliasId, ClassId, EnumId, FunctionId, GenericId, GenericInterfaceId, InterfaceId, ModuleId, VariableId},
    PropName,
};
use crate::error::Error;

use super::{ClassBinding, Transformer, TypeBinding};

type Result<T> = std::result::Result<T, Error>;

impl Transformer{
    fn translate_type_operator(&mut self, ty: &swc::TsType) -> Result<TypeOperator>{
        match ty{
            swc::TsType::TsArrayType(a) => {
                let elem = self.translate_type_operator(&a.elem_type)?;
                return Ok(TypeOperator::ArrayType(Box::new(elem)))
            }
            swc::TsType::TsTupleType(t) => {
                let mut elems = Vec::new();
                for e in &t.elem_types{
                    elems.push(self.translate_type_operator(&e.ty)?);
                }
                return Ok(TypeOperator::TupleType(elems.into()))
            }
            swc::TsType::TsFnOrConstructorType(f) => {
                return self.translate_fn_or_constructor_type_operator(f)
            }
            swc::TsType::TsConditionalType(c) => {
                let test = self.translate_type_operator(&c.check_type)?;
                let constrain = self.translate_type_operator(&c.extends_type)?;
                let left = self.translate_type_operator( &c.true_type)?;
                let right = self.translate_type_operator(&c.false_type)?;

                return Ok(TypeOperator::Conditional { 
                    test: Box::new(test), 
                    constrain: Box::new(constrain), 
                    left: Box::new(left), 
                    right: Box::new(right) 
                })
            }
            swc::TsType::TsInferType(i) => {
                // todo: infer type
                return Err(Error::syntax_error(
                    i.span, 
                    "'infer' declarations are only permitted in the 'extends' clause of a conditional type."
                ))
            }
            swc::TsType::TsImportType(i) => {
                // todo: import type
                return Err(Error::syntax_error(i.span, "import type not supported"))
            }
            swc::TsType::TsIndexedAccessType(i) => {
                if i.readonly{
                    // todo: indexed access readonly
                }

                let obj = self.translate_type_operator(&i.obj_type)?;
                let index = self.translate_type_operator(&i.index_type)?;

                return Ok(TypeOperator::IndexedAccess {
                    target: Box::new(obj), 
                    index_span: i.index_type.span(),
                    index: Box::new(index)
                })
            }
            swc::TsType::TsKeywordType(k) => {
                match k.kind{
                    swc::TsKeywordTypeKind::TsAnyKeyword => Ok(TypeOperator::Type(Type::Any)),
                    swc::TsKeywordTypeKind::TsBigIntKeyword => Ok(TypeOperator::Type(Type::Bigint)),
                    swc::TsKeywordTypeKind::TsBooleanKeyword => Ok(TypeOperator::Type(Type::Bool)),
                    swc::TsKeywordTypeKind::TsIntrinsicKeyword => {
                        // todo: intrinsic type
                        return Err(Error::syntax_error(k.span, "intrinsic types not supported"))
                    }
                    swc::TsKeywordTypeKind::TsNeverKeyword => Ok(TypeOperator::Type(Type::Undefined)),
                    swc::TsKeywordTypeKind::TsNullKeyword => Ok(TypeOperator::Type(Type::Null)),
                    swc::TsKeywordTypeKind::TsNumberKeyword => Ok(TypeOperator::Type(Type::Number)),
                    swc::TsKeywordTypeKind::TsObjectKeyword => Ok(TypeOperator::Type(Type::AnyObject)),
                    swc::TsKeywordTypeKind::TsStringKeyword => Ok(TypeOperator::Type(Type::String)),
                    swc::TsKeywordTypeKind::TsSymbolKeyword => Ok(TypeOperator::Type(Type::Symbol)),
                    swc::TsKeywordTypeKind::TsUndefinedKeyword => Ok(TypeOperator::Type(Type::Undefined)),
                    swc::TsKeywordTypeKind::TsUnknownKeyword => Ok(TypeOperator::Type(Type::Any)),
                    swc::TsKeywordTypeKind::TsVoidKeyword => Ok(TypeOperator::Type(Type::Undefined)),
                }
            }
            swc::TsType::TsLitType(l) => {
                match &l.lit{
                    swc::TsLit::BigInt(b) => {
                        if let Some(b) = b.value.to_i128(){
                            Ok(TypeOperator::Type(Type::LiteralBigint(b)))
                        } else{
                            return Err(Error::syntax_error(b.span, format!("Bigint '{}' has exceeded range of i128", b.value)))
                        }
                    },
                    swc::TsLit::Bool(b) => Ok(TypeOperator::Type(Type::LiteralBool(b.value))),
                    swc::TsLit::Number(n) => Ok(TypeOperator::Type(Type::LiteralNumber(n.value.into()))),
                    swc::TsLit::Str(s) => Ok(TypeOperator::Type(Type::LiteralString(s.value.as_str().into()))),
                    swc::TsLit::Tpl(t) => {
                        // todo: literal template type
                        return Err(Error::syntax_error(t.span, "literal template types not supported"))
                    }
                }
            }
            swc::TsType::TsMappedType(m) => {
                return self.translate_mapped_type_operator(m)
            }
            swc::TsType::TsOptionalType(o) => {
                let ty = self.translate_type_operator(&o.type_ann)?;

                return Ok(TypeOperator::Union(Box::new([ty, TypeOperator::Type(Type::Undefined)])))
            }
            swc::TsType::TsParenthesizedType(p) => return self.translate_type_operator(&p.type_ann),
            swc::TsType::TsRestType(r) => {
                return Err(Error::syntax_error(
                    r.span, 
                    "rest type not supported"
                ))
            }
            swc::TsType::TsThisType(t) => {
                return Ok(TypeOperator::ThisType)
            },
            // typeof operator
            swc::TsType::TsTypeQuery(q) => {
                let ty = self.translate_type_query(q)?;
                return Ok(TypeOperator::Type(ty))
            }
            swc::TsType::TsTypeOperator(o) => {
                let ty = self.translate_type_operator(&o.type_ann)?;
                match o.op {
                    swc::TsTypeOperatorOp::KeyOf => Ok(TypeOperator::KeyOf(Box::new(ty))),
                    swc::TsTypeOperatorOp::ReadOnly => Ok(TypeOperator::ReadOnly(Box::new(ty))),
                    swc::TsTypeOperatorOp::Unique => Ok(TypeOperator::Unique(Box::new(ty)))
                }
            }
        }
    }

    fn translate_fn_or_constructor_type_operator(&mut self,func: &swc::TsFnOrConstructorType) -> Result<TypeOperator>{
        match func{
            swc::TsFnOrConstructorType::TsFnType(func) => {
                if let Some(ty_params) = &func.type_params{
                    // translate generic function
                    let func_ty = self.translate_generic_function_type_operator(
                        &ty_params.params, 
                        &func.params, 
                        &func.type_ann.type_ann
                    )?;

                    return Ok(TypeOperator::GenericFunctionType(Box::new(func_ty)))
                } else{
                    return self.translate_function_type_operator(func)
                }
            }
            swc::TsFnOrConstructorType::TsConstructorType(c) => {
                // todo: constructor type
                return Err(Error::syntax_error(
                    c.span, 
                    "constructor type not supported"
                ))
            }
        }
    }

    fn translate_generic_function_type_operator(&mut self, type_params: &[swc::TsTypeParam], params: &[swc::TsFnParam], return_ty: &swc::TsType) -> Result<GenericFuncType>{
        
        self.context.new_scope();

        let mut ty_params = Vec::new();

        for ty_param in type_params{
            let id = GenericId::new();
            let constrain = match &ty_param.constraint{
                Some(ty) => Some(self.translate_type(&ty)?),
                None => None
            };

            let default = match &ty_param.default{
                Some(ty) => Some(self.translate_type(&ty)?),
                None => None
            };

            ty_params.push(GenericParam{
                id: id,
                name: ty_param.name.sym.to_string(),
                constrain: constrain,
                default: default
            });

            if !self.context.bind_generic(&ty_param.name.sym, id){
                return Err(Error::syntax_error(
                    ty_param.name.span, 
                    format!("duplicated identifier '{}'", ty_param.name.sym)
                ))
            }
        };

        let mut this_ty = TypeOperator::Type(Type::Any);
        let mut func_params = Vec::new();

        for (i, p) in params.iter().enumerate(){
            let ann = match p{
                swc::TsFnParam::Ident(swc::BindingIdent{type_ann, id, ..}) => {
                    let ty = if let Some(ann) = type_ann{
                        self.translate_type_operator(&ann.type_ann)?
                    } else{
                        TypeOperator::Type(Type::Any)
                    };

                    if i == 0 && id.sym.as_str() == "this"{
                        this_ty = ty;
                        continue;
                    } else {
                        ty
                    }
                }
                swc::TsFnParam::Array(swc::ArrayPat{type_ann: Some(ann), ..}) |
                swc::TsFnParam::Object(swc::ObjectPat{type_ann: Some(ann), ..}) |
                swc::TsFnParam::Rest(swc::RestPat{type_ann: Some(ann), ..}) => {
                    self.translate_type_operator(&ann.type_ann)?
                }
                _ => TypeOperator::Type(Type::Any)
            };

            func_params.push(ann);
        }

        let return_ty = self.translate_type_operator(&return_ty)?;

        self.context.end_scope();

        return Ok(GenericFuncType{
            type_params: ty_params,
            this: this_ty,
            params: func_params,
            var_arg: false,
            return_ty: return_ty
        })
    }

    fn translate_function_type_operator(&mut self, func: &swc::TsFnType) -> Result<TypeOperator>{
        let mut this_ty = TypeOperator::Type(Type::Any);
        let mut func_params = Vec::new();

        for (i, p) in func.params.iter().enumerate(){
            let ann = match p{
                swc::TsFnParam::Ident(swc::BindingIdent{type_ann, id, ..}) => {
                    let ty = if let Some(ann) = type_ann{
                        self.translate_type_operator(&ann.type_ann)?
                    } else{
                        TypeOperator::Type(Type::Any)
                    };

                    if i == 0 && id.sym.as_str() == "this"{
                        this_ty = ty;
                        continue;
                    } else {
                        ty
                    }
                }
                swc::TsFnParam::Array(swc::ArrayPat{type_ann: Some(ann), ..}) |
                swc::TsFnParam::Object(swc::ObjectPat{type_ann: Some(ann), ..}) |
                swc::TsFnParam::Rest(swc::RestPat{type_ann: Some(ann), ..}) => {
                    self.translate_type_operator(&ann.type_ann)?
                }
                _ => TypeOperator::Type(Type::Any)
            };

            func_params.push(ann);
        }

        let return_ty = self.translate_type_operator( &func.type_ann.type_ann)?;

        return Ok(TypeOperator::FunctionType { 
            this: Box::new(this_ty), 
            args: func_params.into(), 
            return_ty: Box::new(return_ty) 
        })
    }

    fn translate_mapped_type_operator(&mut self, mapped: &swc::TsMappedType) -> Result<TypeOperator>{
        // create new scope
        self.context.new_scope();

        // translate index type
        let index = if let Some(ty) = &mapped.type_param.constraint{
            self.translate_type_operator(&ty)?
        } else{
            return Err(Error::syntax_error(
                mapped.span, 
                "invalid index expression"
            ))
        };

        // bind the generic
        let name = GenericId::new();

        assert!(self.context.bind_generic(&mapped.type_param.name.sym, name));

        let remap = if let Some(n) = &mapped.name_type{
            Some(Box::new(self.translate_type_operator(&n)?))
        } else{
            None
        };

        // translate property type
        let prop_ty = if let Some(ann) = &mapped.type_ann{
            self.translate_type_operator(&ann)?
        } else{
            TypeOperator::Type(Type::Any)
        };

        // close the scope
        self.context.end_scope();

        return Ok(TypeOperator::Mapped { 
            remove_readonly: mapped.readonly == Some(swc::TruePlusMinus::Minus), 
            add_readonly: mapped.readonly.is_some_and(|r|r != swc::TruePlusMinus::Minus), 
            remove_optional: mapped.optional == Some(swc::TruePlusMinus::Minus), 
            add_optional: mapped.optional.is_some_and(|o|o != swc::TruePlusMinus::Minus), 
            name,
            indexs_span: mapped.type_param.constraint.as_ref().unwrap().span(),
            indexs: Box::new(index),
            name_remap: remap,
            prop_ty: Box::new(prop_ty)
        })
    }
}