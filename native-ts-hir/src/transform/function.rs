use native_js_common::error::Error;
use native_ts_parser::swc_core::common::{Span, Spanned};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::{
    ast::{Expr, FuncType, FunctionParam, Stmt, Type},
    common::{FunctionId, VariableId},
    transform::context::Binding,
};

use super::Transformer;

type Result<T> = std::result::Result<T, Error<Span>>;

impl Transformer {
    pub fn translate_arrow(
        &mut self,
        func: &swc::ArrowExpr,
        expected: Option<&FuncType>,
    ) -> Result<(Expr, Type)> {
        let id = FunctionId::new();

        let mut func_ty = FuncType {
            this_ty: self.this_ty.clone(),
            params: Vec::new(),
            var_arg: false,
            return_ty: Type::Undefined,
        };

        // function params
        for p in func.params.iter() {
            if let Some(ident) = p.as_ident() {
                if ident.type_ann.is_none() {
                    return Err(Error::syntax_error(ident.span, "missing type annotation"));
                }
                let mut ty = self.translate_type(&ident.type_ann.as_ref().unwrap().type_ann)?;

                if ident.optional {
                    ty = ty.union(Type::Undefined);
                }
                func_ty.params.push(ty);
            } else {
                return Err(Error::syntax_error(
                    p.span(),
                    "destructive param is not allowed",
                ));
            }
        }

        let mut return_ty = match &func.return_type {
            Some(ann) => Some(self.translate_type(&ann.type_ann)?),
            None => None,
        };

        self.context.new_function(id);

        match func.body.as_ref() {
            swc::BlockStmtOrExpr::Expr(e) => {
                let (expr, ty) = self.translate_expr(e, return_ty.as_ref())?;

                let mut need_cast = false;

                if return_ty.is_none() {
                    if let Some(expected) = expected {
                        self.type_check(e.span(), &ty, &expected.return_ty)?;
                        return_ty = Some(expected.return_ty.clone());

                        need_cast = expected.return_ty != ty;
                    } else {
                        return_ty = Some(ty);
                    }
                }

                if need_cast {
                    self.context.func().stmts.push(Stmt::Return(Expr::Cast(
                        Box::new(expr),
                        expected.unwrap().return_ty.clone(),
                    )))
                } else {
                    // simply return
                    self.context.func().stmts.push(Stmt::Return(expr));
                }
            }
            swc::BlockStmtOrExpr::BlockStmt(b) => {
                if return_ty.is_none() {
                    return_ty = Some(Type::Undefined);

                    if let Some(expected) = expected {
                        return_ty = Some(expected.return_ty.clone());
                    }
                }
                self.return_ty = return_ty.as_ref().cloned().unwrap();

                self.translate_block_stmt(b, None)?;
            }
        }

        func_ty.return_ty = return_ty.unwrap();

        let func_id = self.context.end_function();
        debug_assert!(func_id == id);

        return Ok((Expr::Closure(id), Type::Function(Box::new(func_ty))));
    }

    pub fn translate_function(
        &mut self,
        id: FunctionId,
        class_this_ty: Option<Type>,
        func: &swc::Function,
    ) -> Result<()> {
        self.context.new_function(id);

        let mut this_ty = Type::Any;

        if let Some(_type_params) = &func.type_params {
            todo!("generic function")
        }

        // clear the params in case of hoisting
        self.context.func().params.clear();

        for (i, p) in func.params.iter().enumerate() {
            if let Some(ident) = p.pat.as_ident() {
                if let Some(ann) = &ident.type_ann {
                    let ty = self.translate_type(&ann.type_ann)?;
                    let id = VariableId::new();

                    if i == 0 && ident.sym.as_ref() == "this" {
                        if class_this_ty.is_some() {
                            return Err(Error::syntax_error(
                                ident.span,
                                "'this' param is not allowed",
                            ));
                        }
                        this_ty = ty;
                        continue;
                    }
                    // declare binding
                    self.context.declare(
                        &ident.sym,
                        Binding::Var {
                            writable: true,
                            redeclarable: true,
                            id: id,
                            ty: ty.clone(),
                        },
                    );
                    // add param
                    self.context
                        .func()
                        .params
                        .push(FunctionParam { id: id, ty: ty });
                } else {
                    return Err(Error::syntax_error(ident.span, "missing type annotation"));
                }
            } else if let Some(r) = p.pat.as_rest() {
                return Err(Error::syntax_error(
                    r.dot3_token,
                    "variable arguments not supported",
                ));
            } else {
                return Err(Error::syntax_error(
                    p.span,
                    "destructive param not supported",
                ));
            }
        }

        if let Some(ty) = class_this_ty {
            this_ty = ty;
        }

        let mut return_ty = if let Some(ann) = &func.return_type {
            self.translate_type(&ann.type_ann)?
        } else {
            Type::Undefined
        };

        if func.is_async {
            return_ty = Type::Promise(Box::new(return_ty));
        }
        if func.is_generator {
            return_ty = Type::Iterator(Box::new(return_ty));
        }

        let old_this_ty = core::mem::replace(&mut self.this_ty, this_ty);
        let old_return_ty = core::mem::replace(&mut self.return_ty, return_ty);

        // translate body
        if let Some(block) = &func.body {
            self.translate_block_stmt(block, None)?
        } else {
            return Err(Error::syntax_error(func.span, "missing function body"));
        }

        let this_ty = core::mem::replace(&mut self.this_ty, old_this_ty);
        let return_ty = core::mem::replace(&mut self.return_ty, old_return_ty);

        let f = self.context.func();
        f.this_ty = this_ty;
        f.return_ty = return_ty;

        let func_id = self.context.end_function();
        debug_assert!(func_id == id);

        return Ok(());
    }
}
