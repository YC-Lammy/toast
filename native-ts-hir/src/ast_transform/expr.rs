use std::rc::Rc;

use native_js_common::error::Error;

use swc_common::Spanned;
use swc_ecmascript::ast as swc;

use crate::{context::Binding, untyped_hir as uhir};

use super::{Result, Translater};

impl Translater {
    pub fn translate_expr(&mut self, expr: &swc::Expr) -> Result<uhir::Expr> {
        match expr {
            swc::Expr::Array(a) => return self.translate_array_lit(a),
            swc::Expr::Arrow(a) => return self.translate_arrow(a),
            swc::Expr::Assign(a) => return self.translate_assign(a),
            swc::Expr::Await(a) => {
                let value = self.translate_expr(&a.arg)?;
                return Ok(uhir::Expr::Await {
                    span: a.span,
                    value: value.into(),
                });
            }
            swc::Expr::Bin(b) => return self.translate_bin_expr(b),
            swc::Expr::Call(c) => return self.translate_call(c),
            swc::Expr::Class(c) => {
                return Err(Error::syntax_error(c.class.span, "class expression is not allowed. classes are not objects"));
            }
            swc::Expr::Cond(c) => {
                return self.translate_cond_expr(c)
            }
            swc::Expr::Fn(f) => {
                if let Some(p) = &f.function.type_params{
                    return Err(Error::syntax_error(p.span, "type params are not allowed in fucntion expression"))
                }
                let mut func = self.translate_function(&f.function)?;

                if let Some(name) = &f.ident{
                    func.name = name.sym.to_string();
                }

                return Ok(uhir::Expr::Function(Rc::new(func)));
            }
            swc::Expr::Ident(id) => {
                return self.translate_ident(id)
            }
            swc::Expr::Invalid(i) => return Err(Error::syntax_error(i.span, "invalid expression")),
            swc::Expr::Lit(l) => {}
            swc::Expr::Member(m) => {
                return self.translate_member(m)
            }
            swc::Expr::MetaProp(m) => {}
            swc::Expr::New(n) => {}
            swc::Expr::New(n) => {}
            swc::Expr::Object(o) => {}
            swc::Expr::OptChain(o) => {}
            swc::Expr::Paren(p) => {}
            swc::Expr::PrivateName(p) => {
                return Err(Error::syntax_error(p.span, "invalid expression"))
            }
            swc::Expr::Seq(s) => {}
            swc::Expr::SuperProp(s) => {}
            swc::Expr::TaggedTpl(t) => {}
            swc::Expr::This(t) => {}
            swc::Expr::Tpl(t) => {}
            swc::Expr::TsAs(t) => {}
            swc::Expr::TsConstAssertion(c) => {}
            swc::Expr::TsNonNull(n) => {}
            swc::Expr::TsSatisfies(s) => {}
            swc::Expr::TsTypeAssertion(t) => {}
            swc::Expr::TsInstantiation(i) => {}
            swc::Expr::Unary(u) => {}
            swc::Expr::Update(u) => {}
            swc::Expr::Yield(y) => {}

            swc::Expr::JSXElement(_)
            | swc::Expr::JSXEmpty(_)
            | swc::Expr::JSXFragment(_)
            | swc::Expr::JSXMember(_)
            | swc::Expr::JSXNamespacedName(_) => {
                return Err(Error::syntax_error(expr.span(), "JSX not supported"))
            }
        };
    }

    pub fn translate_array_lit(&mut self, a: &swc::ArrayLit) -> Result<uhir::Expr> {
        let mut elems = Vec::new();

        for e in &a.elems {
            if let Some(e) = e {
                if e.spread.is_some() {
                    todo!()
                }
                elems.push(self.translate_expr(&e.expr)?)
            } else {
                elems.push(uhir::Expr::Undefined)
            };
        }

        return Ok(uhir::Expr::Array {
            span: a.span,
            values: elems,
        });
    }

    pub fn translate_assign(&mut self, a: &swc::AssignExpr) -> Result<uhir::Expr> {
        let right = self.translate_expr(&a.right)?;

        let left =
        if let swc::PatOrExpr::Pat(p) = &a.left{
            if a.op != swc::AssignOp::Assign && !p.is_ident(){
                return Err(Error::syntax_error(a.span, "invalid left-hand side assignment"))
            }
            if a.op == swc::AssignOp::Assign{
                return self.translate_pat_assign(p, right)
            }

            let ident = a.left.as_ident().unwrap();

            match self.context.find(&ident.sym){
                Some(Binding::Let { id, ty })
                | Some(Binding::Var { id, ty }) => {
                    uhir::MemberOrVar::Var {
                        span: ident.span,
                        name: ident.sym.to_string(), 
                        id: id.clone(), 
                        ty: ty.clone() 
                    }
                },
                Some(Binding::AwaitUsing { .. })
                | Some(Binding::Using{..})
                | Some(Binding::Const{..}) => {
                    return Err(Error::syntax_error(ident.span, "cannot assign to constant variable"))
                }
                _ => return Err(Error::syntax_error(ident.span, "invalid left-hand side assignment"))
            }
        } else{
            let left = a.left.as_expr().unwrap();

            if let Some(m) = left.as_member(){
                // left hand side must be a member expression
                if let uhir::Expr::Member { span, obj, prop, is_optchain } = self.translate_member(m)?{

                    // member cannot be optchained
                    if is_optchain{
                        return Err(Error::syntax_error(left.span(), "invalid left-hand side assignment"))
                    }
                    uhir::MemberOrVar::Member { span, obj, prop }
                } else{
                    // not a member expression
                    return Err(Error::syntax_error(left.span(), "invalid left-hand side assignment"))
                }
            } else{
                // not a member expression
                return Err(Error::syntax_error(left.span(), "invalid left-hand side assignment"))
            }
        };

        return Ok(uhir::Expr::Assign { 
            span: a.span, 
            assign_op: a.op.into(), 
            target: left, 
            value: right.into(),
        })
    }

    pub fn translate_pat_assign(&mut self, pat: &swc::Pat, value: uhir::Expr) -> Result<uhir::Expr>{
        match pat{
            swc::Pat::Ident(ident) => {
                match self.context.find(&ident.sym){
                    Some(Binding::Let { id, ty })
                    | Some(Binding::Var { id, ty }) => {
                        return Ok(uhir::Expr::Assign { 
                            span: ident.span, 
                            assign_op: uhir::AssignOp::Assign, 
                            target: uhir::MemberOrVar::Var { 
                                span: ident.span, 
                                name: ident.sym.to_string(), 
                                id: *id, 
                                ty: ty.clone() 
                            }, 
                            value: value.into(),
                        });
                    }
                    Some(Binding::AwaitUsing{..})
                    | Some(Binding::Using{..})
                    | Some(Binding::Const{..}) => {
                        return Err(Error::syntax_error(ident.span, "cannot assign to constant variable"))
                    }
                    _ => return Err(Error::syntax_error(ident.span, "invalid left-hand side assignment"))
                }
            },
            _ => return Err(Error::syntax_error(pat.span(), "invalid left-hand sid assignment"))
        }
    }

    pub fn translate_bin_expr(&mut self, b: &swc::BinExpr) -> Result<uhir::Expr> {
        let right = self.translate_expr(&b.right)?;

        match b.op {
            swc::BinaryOp::In => {
                // left hand side can be private name
                if let Some(p) = b.left.as_private_name() {
                    return Ok(uhir::Expr::PrivateNameIn {
                        span: b.span,
                        name: p.id.sym.to_string(),
                        value: Box::new(right),
                    });
                }
            }
            _ => {}
        }

        let left = self.translate_expr(&b.left)?;

        let op: uhir::BinOp = b.op.into();

        return Ok(uhir::Expr::Bin {
            span: b.span,
            op: op,
            left: left.into(),
            right: right.into(),
        });
    }

    pub fn translate_callee(&mut self, callee: &swc::Callee) -> Result<uhir::Callee> {
        match callee{
            swc::Callee::Super(_) => Ok(uhir::Callee::Super),
            swc::Callee::Import(i) => {
                return Err(Error::syntax_error(i.span, "dynamic import not allowed"))
            }
            swc::Callee::Expr(e) => {
                if let Some(id) = e.as_ident(){
                    match self.context.find(&id.sym){
                        Some(Binding::Function(f)) => {
                            return Ok(uhir::Callee::Function(f.clone()))
                        }
                        _ => {}
                    };
                    return Ok(uhir::Callee::Expr(Box::new(self.translate_ident(id)?)))
                };

                if let Some(m) = e.as_member(){
                    let m = self.translate_member(m)?;

                    if let uhir::Expr::Member { span, obj, prop, is_optchain } = m{
                        return Ok(uhir::Callee::Member { span, obj, prop, is_optchain })
                    }

                    if let uhir::Expr::ClassMember { span, class, prop, is_optchain } = m{
                        return Ok(uhir::Callee::ClassMember { span, class, prop, is_optchain })
                    }
                }

                return Err(Error::syntax_error(callee.span(), "expected function"))
            }
        }
    }

    pub fn translate_call(&mut self, call: &swc::CallExpr) -> Result<uhir::Expr> {
        // translate the callee
        let callee = self.translate_callee(&call.callee)?;

        // translate the type arguments
        let mut type_args = Vec::new();

        if let Some(ty_args) = &call.type_args{
            for t in &ty_args.params{
                type_args.push(self.translate_ty(&t)?)
            }
        };

        if !type_args.is_empty(){

            match &callee{
                uhir::Callee::Expr(_) 
                | uhir::Callee::Super => return Err(Error::syntax_error(call.span, "callee expected 0 type arguments")),
                uhir::Callee::Function(f) => {
                    let min_generic = f.ty.minimum_generics();

                    if type_args.len() < min_generic{
                        return Err(Error::syntax_error(call.span, format!("function '{}' expected {} type arguments, {} were given", f.name, min_generic, type_args.len())))
                    }

                    if f.is_definite && type_args.len() > f.ty.generics.len(){
                        return Err(Error::syntax_error(call.span, format!("function '{}' expected {} type arguments, {} were given", f.name, f.ty.generics.len(), type_args.len())))
                    }
                },
                _ => {}
            }
        };

        // translate the arguments
        let mut args = Vec::new();

        for arg in &call.args{
            if let Some(sp) = arg.spread{
                return Err(Error::syntax_error(sp, "variable arguments not allowed"))
            }
            args.push(self.translate_expr(&arg.expr)?);
        }

        return Ok(uhir::Expr::Call { 
            span: call.span, 
            callee: callee, 
            is_optchain: false, 
            type_args: type_args, 
            args: args
        })
    }

    pub fn translate_cond_expr(&mut self, cond: &swc::CondExpr) -> Result<uhir::Expr>{
        let test = self.translate_expr(&cond.test)?;
        let cons = self.translate_expr(&cond.cons)?;
        let alt = self.translate_expr(&cond.alt)?;

        return Ok(uhir::Expr::Ternary { 
            span: cond.span, 
            test: test.into(), 
            left: cons.into(), 
            right: alt.into(),
        })
    }

    pub fn translate_ident(&mut self, ident: &swc::Ident) -> Result<uhir::Expr>{

    }

    pub fn translate_member(&mut self, member: &swc::MemberExpr) -> Result<uhir::Expr>{

    }

    pub fn translate_arrow(&mut self, arrow: &swc::ArrowExpr) -> Result<uhir::Expr> {}

    pub fn translate_ts_instantiation(
        &mut self,
        inst: &swc::TsInstantiation,
    ) -> Result<uhir::Expr> {
        let mut type_args = Vec::new();

        if let Some(id) = inst.expr.as_ident() {
            match self.context.find(&id.sym) {
                Some(Binding::Class(c)) => {
                    return Err(Error::syntax_error(id.span, "expected value, found class"))
                }
                Some(Binding::Interface(_)) => {
                    return Err(Error::syntax_error(
                        id.span,
                        "expected value, found interface",
                    ))
                }
                Some(Binding::Function(f)) => {
                    return Ok(uhir::Expr::TypedFunction {
                        span: inst.span,
                        type_args: type_args,
                        func: f.clone(),
                    })
                }
                _ => {}
            }
        };

        return Err(Error::syntax_error(
            inst.span,
            "invalid left hand side expression",
        ));
    }
}
