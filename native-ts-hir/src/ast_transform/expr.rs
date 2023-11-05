use native_js_common::rc::Rc;

use native_js_common::error::Error;

use num_traits::ToPrimitive;
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
                return Err(Error::syntax_error(
                    c.class.span,
                    "class expression is not allowed. classes are not objects",
                ));
            }
            swc::Expr::Cond(c) => return self.translate_cond_expr(c),
            swc::Expr::Fn(f) => {
                if let Some(p) = &f.function.type_params {
                    return Err(Error::syntax_error(
                        p.span,
                        "type params are not allowed in fucntion expression",
                    ));
                }
                let mut func = self.translate_function(&f.function)?;

                if let Some(name) = &f.ident {
                    func.name = name.sym.to_string();
                }

                return Ok(uhir::Expr::Function {
                    span: f.span(),
                    type_args: Vec::new(),
                    func: Rc::new(func),
                });
            }
            swc::Expr::Ident(id) => return self.translate_ident(id),
            swc::Expr::Invalid(i) => return Err(Error::syntax_error(i.span, "invalid expression")),
            swc::Expr::Lit(l) => {
                return self.translate_litral_expr(l)
            }
            swc::Expr::Member(m) => return self.translate_member(m, false),
            swc::Expr::MetaProp(m) => {
                return Ok(match m.kind{
                    swc::MetaPropKind::ImportMeta => uhir::Expr::ImportMeta,
                    swc::MetaPropKind::NewTarget => uhir::Expr::NewTarget
                });
            }
            swc::Expr::New(n) => return self.translate_new_expr(n),
            swc::Expr::Object(o) => {
                return Err(Error::syntax_error(o.span, "object literal not allowed. use classes instead"))
            }
            swc::Expr::OptChain(o) => {
                return self.translae_optchain(o)
            }
            swc::Expr::Paren(p) => return self.translate_expr(&p.expr),
            swc::Expr::PrivateName(p) => {
                return Err(Error::syntax_error(p.span, "invalid expression"))
            }
            swc::Expr::Seq(s) => {
                let mut seq = Vec::new();
                for e in &s.exprs{
                    seq.push(self.translate_expr(&e)?);
                }
                return Ok(uhir::Expr::Seq { 
                    span: s.span, 
                    exprs: seq
                })
            }
            swc::Expr::SuperProp(s) => {
                return Err(Error::syntax_error(s.span, "super property not supported, use 'this' instead"))
            }
            swc::Expr::This(t) => {
                return Ok(uhir::Expr::This(t.span))
            }
            swc::Expr::TaggedTpl(t) => {
                return Err(Error::syntax_error(t.span, "not supported"))
            }
            swc::Expr::Tpl(t) => {
                return Err(Error::syntax_error(t.span, "template not supported"))
            }
            swc::Expr::TsAs(t) => {
                let value = self.translate_expr(&t.expr)?;
                let ty = self.translate_ty(&t.type_ann)?;

                return Ok(uhir::Expr::Cast { 
                    span: t.span, 
                    value: value.into(), 
                    to_ty: ty 
                })
            }
            swc::Expr::TsConstAssertion(c) => {
                return Err(Error::syntax_error(c.span, "not supported"))
            }
            swc::Expr::TsNonNull(n) => {
                return Err(Error::syntax_error(n.span, "not supported"))
            }
            swc::Expr::TsSatisfies(s) => {
                return Err(Error::syntax_error(s.span, "not supported"))
            }
            swc::Expr::TsTypeAssertion(t) => {
                return Err(Error::syntax_error(t.span, "type assertion not supported"))
            }
            swc::Expr::TsInstantiation(i) => {
                return self.translate_ts_instantiation(i)
            }
            swc::Expr::Unary(u) => {
                let arg = self.translate_expr(&u.arg)?;
                let op = u.op.into();

                return Ok(uhir::Expr::UnaryOp { 
                    span: u.span, 
                    op: op, 
                    value: arg.into()
                })
            }
            swc::Expr::Update(u) => {
                // an update operation can only update member or variable
                let left = self.translate_member_or_var(&u.arg)?;

                let op =
                match u.op{
                    swc::UpdateOp::MinusMinus => if u.prefix{
                        uhir::UpdateOp::PrefixSub
                    } else{
                        uhir::UpdateOp::SuffixSub
                    }
                    swc::UpdateOp::PlusPlus => if u.prefix{
                        uhir::UpdateOp::PrefixAdd
                    } else{
                        uhir::UpdateOp::SuffixAdd
                    }
                };

                return Ok(uhir::Expr::Update { 
                    span: u.span, 
                    target: left, 
                    op: op
                });
            }
            swc::Expr::Yield(y) => {
                let value = if let Some(arg) = &y.arg{
                    self.translate_expr(arg)?
                } else{
                    uhir::Expr::Undefined
                };

                return Ok(uhir::Expr::Yield { 
                    span: y.span, 
                    delegate: y.delegate, 
                    value: value.into()
                })
            }

            swc::Expr::JSXElement(_)
            | swc::Expr::JSXEmpty(_)
            | swc::Expr::JSXFragment(_)
            | swc::Expr::JSXMember(_)
            | swc::Expr::JSXNamespacedName(_) => {
                return Err(Error::syntax_error(expr.span(), "JSX not supported"))
            }
        };
    }

    pub fn translate_litral_expr(&mut self, lit: &swc::Lit) -> Result<uhir::Expr>{
        match lit{
            swc::Lit::BigInt(b) => {
                return Ok(uhir::Expr::BigInt(b.value.to_i128().expect("i128 overflow")))
            }
            swc::Lit::Bool(b) => Ok(uhir::Expr::Bool(b.value)),
            swc::Lit::Null(_) => Ok(uhir::Expr::Null),
            swc::Lit::Num(n) => {
                if n.value as i32 as f64 == n.value{
                    return Ok(uhir::Expr::Integer(n.value as i32))
                }
                return Ok(uhir::Expr::Number(n.value))
            }
            swc::Lit::Regex(r) => {
                return Ok(uhir::Expr::Regex { reg: r.exp.to_string(), flags: r.flags.to_string() })
            }
            swc::Lit::Str(s) => {
                return Ok(uhir::Expr::String(s.value.to_string()))
            }
            swc::Lit::JSXText(j) => {
                return Err(Error::syntax_error(j.span, "JSX not supported"))
            }
        }
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

        let left = if let swc::PatOrExpr::Pat(p) = &a.left {
            if a.op != swc::AssignOp::Assign && !p.is_ident() {
                return Err(Error::syntax_error(
                    a.span,
                    "invalid left-hand side assignment",
                ));
            }
            if a.op == swc::AssignOp::Assign {
                return self.translate_pat_assign(p, right);
            }

            let ident = a.left.as_ident().unwrap();

            match self.context.find(&ident.sym) {
                Some(Binding::Let { id, ty }) | Some(Binding::Var { id, ty }) => {
                    uhir::MemberOrVar::Var {
                        span: ident.span,
                        name: ident.sym.to_string(),
                        id: id.clone(),
                        ty: ty.clone(),
                    }
                }
                Some(Binding::AwaitUsing { .. })
                | Some(Binding::Using { .. })
                | Some(Binding::Const { .. }) => {
                    return Err(Error::syntax_error(
                        ident.span,
                        "cannot assign to constant variable",
                    ))
                }
                _ => {
                    return Err(Error::syntax_error(
                        ident.span,
                        "invalid left-hand side assignment",
                    ))
                }
            }
        } else {
            let left = a.left.as_expr().unwrap();

            self.translate_member_or_var(left)?
        };

        return Ok(uhir::Expr::Assign {
            span: a.span,
            assign_op: a.op.into(),
            target: left,
            value: right.into(),
        });
    }

    pub fn translate_pat_assign(
        &mut self,
        pat: &swc::Pat,
        value: uhir::Expr,
    ) -> Result<uhir::Expr> {
        match pat {
            swc::Pat::Ident(ident) => match self.context.find(&ident.sym) {
                Some(Binding::Let { id, ty }) | Some(Binding::Var { id, ty }) => {
                    return Ok(uhir::Expr::Assign {
                        span: ident.span,
                        assign_op: uhir::AssignOp::Assign,
                        target: uhir::MemberOrVar::Var {
                            span: ident.span,
                            name: ident.sym.to_string(),
                            id: *id,
                            ty: ty.clone(),
                        },
                        value: value.into(),
                    });
                }
                Some(Binding::AwaitUsing { .. })
                | Some(Binding::Using { .. })
                | Some(Binding::Const { .. }) => {
                    return Err(Error::syntax_error(
                        ident.span,
                        "cannot assign to constant variable",
                    ))
                }
                _ => {
                    return Err(Error::syntax_error(
                        ident.span,
                        "invalid left-hand side assignment",
                    ))
                }
            },
            _ => {
                return Err(Error::syntax_error(
                    pat.span(),
                    "invalid left-hand sid assignment",
                ))
            }
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
            swc::BinaryOp::InstanceOf => {
               let ty = self.translate_type_expr(&b.left, vec![])?;

               return Ok(uhir::Expr::InstanceOf { 
                    span: b.span, 
                    value: right.into(), 
                    ty: ty 
                })
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
        match callee {
            swc::Callee::Super(_) => Ok(uhir::Callee::Super),
            swc::Callee::Import(i) => {
                return Err(Error::syntax_error(i.span, "dynamic import not allowed"))
            }
            swc::Callee::Expr(e) => {
                if let Some(id) = e.as_ident() {
                    match self.context.find(&id.sym) {
                        Some(Binding::Function(f)) => return Ok(uhir::Callee::Function(f.clone())),
                        _ => {}
                    };
                    return Ok(uhir::Callee::Expr(Box::new(self.translate_ident(id)?)));
                };

                if let Some(m) = e.as_member() {
                    let m = self.translate_member(m, false)?;

                    if let uhir::Expr::Member {
                        span,
                        obj,
                        prop,
                        type_args,
                        is_optchain,
                    } = m
                    {
                        debug_assert!(type_args.is_empty());

                        return Ok(uhir::Callee::Member {
                            span,
                            obj,
                            prop,
                            is_optchain,
                        });
                    }

                    if let uhir::Expr::ClassMember {
                        span,
                        class,
                        prop,
                        type_args,
                        is_optchain,
                    } = m
                    {
                        debug_assert!(type_args.is_empty());

                        return Ok(uhir::Callee::ClassMember {
                            span,
                            class,
                            prop,
                            is_optchain,
                        });
                    }
                }

                // fallback to dynamic call
                return Ok(uhir::Callee::Expr(self.translate_expr(&e)?.into()))
            }
        }
    }

    pub fn translate_call(&mut self, call: &swc::CallExpr) -> Result<uhir::Expr> {
        // translate the callee
        let callee = self.translate_callee(&call.callee)?;

        // translate the type arguments
        let mut type_args = Vec::new();

        if let Some(ty_args) = &call.type_args {
            for t in &ty_args.params {
                type_args.push(self.translate_ty(&t)?)
            }
        };

        if !type_args.is_empty() {
            match &callee {
                uhir::Callee::Expr(_) | uhir::Callee::Super => {
                    return Err(Error::syntax_error(
                        call.span,
                        "callee expected 0 type arguments",
                    ))
                }
                uhir::Callee::Function(f) => {
                    let min_generic = f.ty.minimum_generics();

                    if type_args.len() < min_generic {
                        return Err(Error::syntax_error(
                            call.span,
                            format!(
                                "function '{}' expected {} type arguments, {} were given",
                                f.name,
                                min_generic,
                                type_args.len()
                            ),
                        ));
                    }

                    if f.is_definite && type_args.len() > f.ty.generics.len() {
                        return Err(Error::syntax_error(
                            call.span,
                            format!(
                                "function '{}' expected {} type arguments, {} were given",
                                f.name,
                                f.ty.generics.len(),
                                type_args.len()
                            ),
                        ));
                    }
                }
                _ => {}
            }
        };

        // translate the arguments
        let mut args = Vec::new();

        for arg in &call.args {
            if let Some(sp) = arg.spread {
                return Err(Error::syntax_error(sp, "variable arguments not allowed"));
            }
            args.push(self.translate_expr(&arg.expr)?);
        }

        return Ok(uhir::Expr::Call {
            span: call.span,
            callee: callee,
            is_optchain: false,
            type_args: type_args,
            args: args,
        });
    }

    pub fn translate_cond_expr(&mut self, cond: &swc::CondExpr) -> Result<uhir::Expr> {
        let test = self.translate_expr(&cond.test)?;
        let cons = self.translate_expr(&cond.cons)?;
        let alt = self.translate_expr(&cond.alt)?;

        return Ok(uhir::Expr::Ternary {
            span: cond.span,
            test: test.into(),
            left: cons.into(),
            right: alt.into(),
        });
    }

    pub fn translate_ident(&mut self, ident: &swc::Ident) -> Result<uhir::Expr> {
        match self.context.find(&ident.sym){
            Some(Binding::AwaitUsing { id, ty })
            | Some(Binding::Using { id, ty })
            | Some(Binding::Const { id, ty })
            | Some(Binding::Let { id, ty })
            | Some(Binding::Var { id, ty }) => {
                return Ok(uhir::Expr::ReadVar { 
                    span: ident.span, 
                    name: ident.sym.to_string(), 
                    id: *id, 
                    ty: ty.clone()
                })
            },
            _ => {
                return Err(Error::syntax_error(ident.span, format!("invalid expression, identifier '{}' is not a variable", ident.sym)))
            }
        }
    }

    // returns am member, class member or a variable
    pub fn translate_member_or_var(&mut self, expr: &swc::Expr) -> Result<uhir::MemberOrVar<uhir::Type, uhir::Function>>{
        match expr{
            swc::Expr::Ident(ident) => {
                match self.context.find(&ident.sym){
                    Some(Binding::Let { id, ty })
                    | Some(Binding::Var { id, ty }) => {
                        return Ok(uhir::MemberOrVar::Var { 
                            span: ident.span, 
                            name: ident.sym.to_string(), 
                            id: *id, 
                            ty: ty.clone() 
                        })
                    },
                    Some(Binding::AwaitUsing { .. })
                    | Some(Binding::Using{..})
                    | Some(Binding::Const{..}) => {
                        return Err(Error::syntax_error(ident.span, "cannot assign to constant variables"))
                    }
                    _ => {
                        return Err(Error::syntax_error(ident.span, "invalid left-hand side assignment"))
                    }
                }
            }
            swc::Expr::Member(member) => {
                let propname =
                match &member.prop{
                    swc::MemberProp::Computed(c) => {
                        self.translate_computed_prop_name(&c.expr)?
                    }
                    swc::MemberProp::Ident(id) => {
                        uhir::PropName::Ident(id.sym.to_string())
                    }
                    swc::MemberProp::PrivateName(p) => {
                        uhir::PropName::Private(p.id.sym.to_string())
                    }
                };
                // try to translate as a value
                let obj = self.translate_expr(&member.obj);

                // failed to translate expression, expression may be a type
                if obj.is_err(){
                    // try to translate as a type
                    let ty = self.translate_type_expr(&member.obj, Vec::new());

                    // it is not a type
                    if ty.is_err(){
                        // return the original error
                        return Err(obj.err().unwrap())
                    }

                    // it is a type member
                    return Ok(uhir::MemberOrVar::ClassMember { 
                        span: member.span, 
                        class: ty.ok().unwrap(), 
                        prop: propname,
                    })
                };

                let obj = obj?;

                // return a normal member expression
                return Ok(uhir::MemberOrVar::Member { 
                    span: member.span, 
                    obj: Box::new(obj), 
                    prop: propname,
                })
            },
            _ => return Err(Error::syntax_error(expr.span(), "invalid left-hand side assignment"))
        }
    }

    pub fn translate_member(&mut self, member: &swc::MemberExpr, is_optchain: bool) -> Result<uhir::Expr> {
        let propname =
        match &member.prop{
            swc::MemberProp::Computed(c) => {
                self.translate_computed_prop_name(&c.expr)?
            }
            swc::MemberProp::Ident(id) => {
                uhir::PropName::Ident(id.sym.to_string())
            }
            swc::MemberProp::PrivateName(p) => {
                uhir::PropName::Private(p.id.sym.to_string())
            }
        };
        let obj = self.translate_expr(&member.obj);

        // failed to translate expression, expression may be a type
        if obj.is_err(){
            // try to translate as a type
            let ty = self.translate_type_expr(&member.obj, Vec::new());

            // it is not a type
            if ty.is_err(){
                // return the original error
                return Err(obj.err().unwrap())
            }

            // it is a type member
            return Ok(uhir::Expr::ClassMember { 
                span: member.span, 
                class: ty.ok().unwrap(), 
                prop: propname, 
                type_args: Vec::new(),
                is_optchain: is_optchain
            })
        };

        let obj = obj?;

        // return a normal member expression
        return Ok(uhir::Expr::Member { 
            span: member.span, 
            obj: Box::new(obj), 
            prop: propname, 
            type_args: Vec::new(),
            is_optchain: is_optchain
        })
    }

    pub fn translae_optchain(&mut self, o: &swc::OptChainExpr) -> Result<uhir::Expr>{
        match o.base.as_ref(){
            swc::OptChainBase::Member(m) => {
                return self.translate_member(&m, true);
            }
            swc::OptChainBase::Call(c) => {
                let callee = self.translate_expr(&c.callee)?;

                if let Some(ty_args) = &c.type_args{
                    return Err(Error::syntax_error(ty_args.span, "type arguments in dynamic call is not allowed"))
                }

                let mut args = Vec::new();

                for p in &c.args{
                    if let Some(spread) = p.spread{
                        return Err(Error::syntax_error(spread, "variable arguments not supported"))
                    }
                    args.push(self.translate_expr(&p.expr)?);
                }

                return Ok(uhir::Expr::Call { 
                    span: c.span, 
                    callee: uhir::Callee::Expr(callee.into()), 
                    is_optchain: true, 
                    type_args: Vec::new(), 
                    args: args
                })
            }
        }
    }

    // new Class()
    pub fn translate_new_expr(&mut self, n: &swc::NewExpr) -> Result<uhir::Expr>{
        let mut type_args = Vec::new();

        // translate type arguments
        if let Some(args) = &n.type_args{
            for arg in &args.params{
                let ty = self.translate_ty(arg)?;
                type_args.push(ty);
            }
        };

        // resolve type
        let class = self.translate_type_expr(&n.callee, type_args)?;

        let mut args = Vec::new();

        if let Some(p) = &n.args{
            for arg in p{
                if let Some(spread) = arg.spread{
                    return Err(Error::syntax_error(spread, "variable argumens not allowed"))
                }

                args.push(self.translate_expr(&arg.expr)?);
            };
        };

        return Ok(uhir::Expr::New { 
            span: n.span, 
            callee: class, 
            args: args 
        })
    }

    pub fn translate_ts_instantiation(
        &mut self,
        inst: &swc::TsInstantiation,
    ) -> Result<uhir::Expr> {
        let mut type_args = Vec::new();

        for arg in &inst.type_args.params{
            type_args.push(self.translate_ty(&arg)?);
        }

        if let Some(id) = inst.expr.as_ident() {
            match self.context.find(&id.sym) {
                Some(Binding::Class(_)) => {
                    return Err(Error::syntax_error(id.span, "expected value, found class"))
                }
                Some(Binding::Interface(_)) => {
                    return Err(Error::syntax_error(
                        id.span,
                        "expected value, found interface",
                    ))
                }
                // only functions are allowed
                Some(Binding::Function(f)) => {
                    return Ok(uhir::Expr::Function {
                        span: inst.span,
                        type_args: type_args,
                        func: f.clone(),
                    })
                }
                _ => {
                    return Err(Error::syntax_error(id.span, "expected function"))
                }
            }
        };

        if let Some(member) = inst.expr.as_member(){
            let propname =
            match &member.prop{
                swc::MemberProp::Computed(c) => {
                    self.translate_computed_prop_name(&c.expr)?
                }
                swc::MemberProp::Ident(id) => {
                    uhir::PropName::Ident(id.sym.to_string())
                }
                swc::MemberProp::PrivateName(p) => {
                    uhir::PropName::Private(p.id.sym.to_string())
                }
            };
            let obj = self.translate_expr(&member.obj);

            // failed to translate expression, expression may be a type
            if obj.is_err(){
                // try to translate as a type
                let ty = self.translate_type_expr(&member.obj, Vec::new());

                // it is not a type
                if ty.is_err(){
                    // return the original error
                    return Err(obj.err().unwrap())
                }

                // it is a type member
                return Ok(uhir::Expr::ClassMember { 
                    span: member.span, 
                    class: ty.ok().unwrap(), 
                    prop: propname, 
                    type_args: type_args,
                    is_optchain: false 
                })
            };

            let obj = obj?;

            // return a normal member expression
            return Ok(uhir::Expr::Member { 
                span: member.span, 
                obj: Box::new(obj), 
                prop: propname, 
                type_args: type_args,
                is_optchain: false
            })
        }

        return Err(Error::syntax_error(
            inst.span,
            "invalid left-hand side expression",
        ));
    }
}
