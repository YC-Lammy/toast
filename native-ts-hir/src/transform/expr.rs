use native_js_common::error::Error;

use num_traits::ToPrimitive;

use native_ts_parser::swc_core::common::{Span, Spanned};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::{
    ast::{Callee, Expr, FuncType, PropNameOrExpr, Type},
    common::FunctionId,
    PropName,
};

use super::{context::Binding, Transformer};

type Result<T> = std::result::Result<T, Error<Span>>;

impl Transformer {
    pub fn translate_expr(
        &mut self,
        expr: &swc::Expr,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        let (mut e, ty) = match expr {
            swc::Expr::Array(a) => self.translate_array_expr(a, expected_ty)?,
            swc::Expr::Arrow(a) => {
                let expected = expected_ty.and_then(|ty| {
                    if let Type::Function(f) = ty {
                        Some(f.as_ref())
                    } else {
                        None
                    }
                });
                self.translate_arrow(a, expected)?
            }
            swc::Expr::Assign(a) => self.translate_assign(a)?,
            swc::Expr::Await(a) => {
                let (e, mut ty) = self.translate_expr(&a.arg, None)?;

                if let Type::Promise(p) = ty {
                    ty = *p;
                };

                if let Type::Union(u) = ty {
                    let mut v = Vec::with_capacity(u.len());

                    for t in u.iter() {
                        if let Type::Promise(p) = t {
                            v.push(p.as_ref().clone());
                        } else {
                            v.push(t.clone())
                        }
                    }
                    ty = Type::Union(v.into_boxed_slice())
                }

                (Expr::Await(Box::new(e)), ty)
            }
            swc::Expr::Bin(b) => self.translate_bin(b)?,
            swc::Expr::Call(c) => self.translate_call(c)?,
            swc::Expr::Class(c) => {
                return Err(Error::syntax_error(
                    c.span(),
                    "class expression not allowed",
                ))
            }
            swc::Expr::Cond(cond) => self.translate_cond(cond)?,
            swc::Expr::Fn(f) => {
                // generate a new function id
                let id = FunctionId::new();
                self.translate_function(id, None, &f.function)?;

                let ty = self
                    .context
                    .functions
                    .get(&id)
                    .expect("invalid function")
                    .ty();

                (Expr::Closure(id), Type::Function(Box::new(ty)))
            }
            swc::Expr::Ident(id) => self.translate_var_load(id)?,
            swc::Expr::This(_) => (Expr::This, self.this_ty.clone()),
            swc::Expr::Object(o) => {
                return Err(Error::syntax_error(o.span, "object literal not allowed"))
            }
            swc::Expr::Unary(u) => self.translate_unary_expr(u)?,
            swc::Expr::Update(u) => self.translate_update_expr(u)?,
            swc::Expr::Member(m) => self.translate_member(m)?,
            swc::Expr::SuperProp(s) => self.translate_super_prop_expr(s)?,
            swc::Expr::New(n) => self.translate_new_expr(n)?,
            swc::Expr::Seq(s) => self.translate_seq_expr(s, expected_ty)?,
            swc::Expr::Lit(l) => self.translate_lit(l, expected_ty)?,
            swc::Expr::Tpl(_) => todo!(),
            swc::Expr::TaggedTpl(_) => todo!(),
            swc::Expr::Yield(y) => {
                let arg = if let Some(expr) = &y.arg {
                    let r = self.return_ty.clone();
                    let (e, _) = self.translate_expr(expr, Some(&r))?;
                    e
                } else {
                    self.type_check(y.span, &Type::Undefined, &self.return_ty)?;
                    Expr::Undefined
                };
                // for now use undefined
                (Expr::Yield(Box::new(arg)), Type::Undefined)
            }
            swc::Expr::Paren(p) => self.translate_expr(&p.expr, expected_ty)?,

            swc::Expr::MetaProp(m) => {
                return Err(Error::syntax_error(m.span, "meta properties not supported"))
            }
            swc::Expr::PrivateName(p) => {
                return Err(Error::syntax_error(p.span, "invalid expression"))
            }
            swc::Expr::Invalid(i) => return Err(Error::syntax_error(i.span, "invalid expression")),
            swc::Expr::OptChain(o) => self.translate_optchain(o)?,

            swc::Expr::TsAs(a) => {
                let ty = self.translate_type(&a.type_ann)?;
                // translate expr will handle the cast
                let (expr, ty) = self.translate_expr(&a.expr, Some(&ty))?;
                (expr, ty)
            }
            swc::Expr::TsConstAssertion(c) => {
                // does nothing for now
                self.translate_expr(&c.expr, expected_ty)?
            }
            swc::Expr::TsInstantiation(_i) => {
                todo!("generics")
            }
            swc::Expr::TsNonNull(n) => {
                let (expr, mut ty) = self.translate_expr(&n.expr, None)?;

                if ty == Type::Undefined || ty == Type::Null {
                    return Err(Error::syntax_error(
                        n.span,
                        "non null assertion cannot be applied to null types",
                    ));
                }

                if let Type::Union(u) = &ty {
                    let mut v = Vec::new();
                    for ty in u.iter() {
                        if ty != &Type::Undefined && ty != &Type::Null {
                            v.push(ty.clone());
                        }
                    }
                    ty = Type::Union(v.into_boxed_slice());
                }

                (Expr::AssertNonNull(Box::new(expr)), ty)
            }
            swc::Expr::TsSatisfies(s) => {
                let ty = self.translate_type(&s.type_ann)?;
                let (expr, expr_ty) = self.translate_expr(&s.expr, expected_ty)?;
                // check satisfies
                self.type_check(s.span, &expr_ty, &ty)?;

                (expr, expr_ty)
            }
            swc::Expr::TsTypeAssertion(t) => {
                let ty = self.translate_type(&t.type_ann)?;
                // translate expr will handle the cast
                let (expr, ty) = self.translate_expr(&t.expr, Some(&ty))?;
                (expr, ty)
            }

            swc::Expr::JSXElement(_)
            | swc::Expr::JSXEmpty(_)
            | swc::Expr::JSXFragment(_)
            | swc::Expr::JSXMember(_)
            | swc::Expr::JSXNamespacedName(_) => unimplemented!(),
        };

        if let Some(expected) = expected_ty {
            self.type_check(expr.span(), &ty, expected)?;

            if !ty.eq(expected) {
                e = Expr::Cast(Box::new(e), expected.clone())
            }

            return Ok((e, expected.clone()));
        }

        return Ok((e, ty));
    }

    pub fn translate_array_expr(
        &mut self,
        a: &swc::ArrayLit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match expected_ty {
            Some(Type::Array(expected_elem_ty)) => {
                let mut elements = Vec::new();

                for elem in &a.elems {
                    let (span, mut expr, ty) = if let Some(elem) = elem {
                        if let Some(spread) = elem.spread {
                            return Err(Error::syntax_error(
                                spread,
                                "spread expression not allowed",
                            ));
                        }

                        let (e, t) = self.translate_expr(&elem.expr, Some(expected_elem_ty))?;
                        (elem.expr.span(), e, t)
                    } else {
                        (a.span, Expr::Undefined, Type::Undefined)
                    };

                    self.type_check(span, &Type::Undefined, &expected_elem_ty)?;

                    if !expected_elem_ty.as_ref().eq(&ty) {
                        expr = Expr::Cast(Box::new(expr), expected_elem_ty.as_ref().clone());
                    }

                    elements.push(expr);
                }

                return Ok((
                    Expr::Array { values: elements },
                    Type::Array(expected_elem_ty.clone()),
                ));
            }
            Some(Type::Tuple(element_tys)) => {
                if element_tys.len() != a.elems.len() {
                    return Err(Error::syntax_error(
                        a.span,
                        format!(
                            "expected {} elements, {} were given",
                            element_tys.len(),
                            a.elems.len()
                        ),
                    ));
                }

                let mut values = Vec::with_capacity(element_tys.len());

                for (i, elem) in a.elems.iter().enumerate() {
                    let expected = element_tys.get(i).unwrap();

                    let (_span, expr, _ty) = if let Some(elem) = elem {
                        // spread expression not allowed
                        if let Some(spread) = elem.spread {
                            return Err(Error::syntax_error(
                                spread,
                                "spread expression not allowed",
                            ));
                        }
                        // translate value
                        let (e, t) = self.translate_expr(&elem.expr, Some(expected))?;

                        (elem.span(), e, t)
                    } else {
                        // check if undefined fulfills expected type
                        self.type_check(a.span, &Type::Undefined, expected)?;

                        (a.span, Expr::Undefined, Type::Undefined)
                    };

                    // push expression to values
                    values.push(expr);
                }

                // return the tuple
                return Ok((
                    Expr::Tuple { values: values },
                    Type::Tuple(element_tys.clone()),
                ));
            }
            _ => {
                let mut ty: Option<Type> = None;
                let mut values = Vec::new();

                for elem in &a.elems {
                    // translate the element if not empty
                    let (mut expr, mut t) = if let Some(elem) = elem {
                        let (expr, t) = self.translate_expr(&elem.expr, None)?;
                        (expr, t)
                    } else {
                        // if it is empty, it is undefined
                        (Expr::Undefined, Type::Undefined)
                    };

                    // cannot be int, must be casted to number
                    if t == Type::Int {
                        t = Type::Number;
                        expr = Expr::Cast(Box::new(expr), Type::Number);
                    }

                    if let Some(chained) = ty {
                        ty = Some(chained.union(t));
                    } else {
                        ty = Some(t);
                    };
                    values.push(expr);
                }

                let array_ty = ty
                    .map(|t| Type::Array(Box::new(t)))
                    .or_else(|| Some(Type::Array(Box::new(Type::Any))))
                    .unwrap();

                return Ok((Expr::Array { values: values }, array_ty));
            }
        }
    }

    pub fn translate_assign(&mut self, a: &swc::AssignExpr) -> Result<(Expr, Type)> {
        
        let (value, value_ty) = self.translate_expr(&a.right, None)?;
        
        return self.translate_assign_target(a.span,&a.left, a.op, value, value_ty)
    }

    pub fn translate_assign_target(&mut self, span: Span, target: &swc::AssignTarget, op: swc::AssignOp, value: Expr, value_ty: Type) -> Result<(Expr, Type)>{
        match target{
            swc::AssignTarget::Simple(simple) => {
                match simple{
                    swc::SimpleAssignTarget::Ident(id) => self.translate_var_assign(span, &id.id, op, value, value_ty),
                    swc::SimpleAssignTarget::Member(m) => self.translate_member_assign(span, m, op, value, value_ty),
                    swc::SimpleAssignTarget::Invalid(i) => Err(Error::syntax_error(i.span, "invalid assignment target")),
                    _ => todo!("assignment")
                }
            }
            swc::AssignTarget::Pat(p) => {
                match p{
                    swc::AssignTargetPat::Object(pat) => self.translate_object_pat_assign(pat, op, value),
                    swc::AssignTargetPat::Array(pat) => self.translate_array_pat_assign(pat, op, value),
                    swc::AssignTargetPat::Invalid(i) => Err(Error::syntax_error(i.span, "invalid pattern"))
                }
            }
        }
    }

    fn check_assign_op_type(&mut self, span: Span, op: swc::AssignOp, ty: &Type) -> Result<()>{
        match op{
            swc::AssignOp::Assign => {},
            swc::AssignOp::AddAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int && ty != &Type::String{
                    return Err(Error::syntax_error(span, "operator '+=' only accepts number, bigint and string"))
                }
            },
            swc::AssignOp::SubAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '-=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::MulAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '*=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::DivAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '/=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::ExpAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '**=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::ModAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '%=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::LShiftAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '<<=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::RShiftAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '>>=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::ZeroFillRShiftAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '>>>=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::BitAndAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '&=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::BitOrAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '|=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::BitXorAssign => {
                if ty != &Type::Bigint && ty != &Type::Number && ty != &Type::Int{
                    return Err(Error::syntax_error(span, "operator '^=' only accepts number and bigint"))
                }
            }
            swc::AssignOp::NullishAssign => {},
            swc::AssignOp::OrAssign => {},
            swc::AssignOp::AndAssign => {
                if ty != &Type::Bool{
                    return Err(Error::syntax_error(span, "operator '&&=' only accepts boolean"))
                }
            }
        };

        return Ok(())
    }

    pub fn translate_var_assign(&mut self, span: Span, var: &swc::Ident, op: swc::AssignOp, mut value: Expr, value_ty: Type) -> Result<(Expr, Type)>{
        // get the variable id and variable type
        let (varid, var_ty) = if let Some(binding) = self.context.find(&var.sym){
            match binding{
                Binding::Var { writable, redeclarable:_, id, ty } => {
                    // constants are not writable
                    if !*writable{
                        return Err(Error::syntax_error(var.span, format!("variable '{}' is immutable", &var.sym)))
                    }
                    // return id and type
                    (*id, ty.clone())
                }
                // using declare cannot be mutated
                Binding::Using { .. } => {
                    return Err(Error::syntax_error(var.span, format!("variable '{}' is immutable", &var.sym)))
                }
                // all other bindings are not considered variable
                _ => return Err(Error::syntax_error(var.span, format!("identifier '{}' is not a variable", &var.sym)))
            } 
        } else{
            return Err(Error::syntax_error(var.span, format!("undeclared identifier '{}'", var.sym)))
        };

        // type check
        self.type_check(var.span, &value_ty, &var_ty)?;

        if value_ty != var_ty{
            value = Expr::Cast(Box::new(value), var_ty.clone());
        };

        self.check_assign_op_type(span, op, &var_ty)?;

        return Ok((Expr::VarAssign { op: op.into(), variable: varid, value: Box::new(value) }, var_ty))
    }

    pub fn translate_member_assign(&mut self, span: Span, member: &swc::MemberExpr, op: swc::AssignOp, mut value: Expr, value_ty: Type) -> Result<(Expr, Type)>{
        let (member_expr, member_ty) = self.translate_member(member)?;

        self.type_check(span, &value_ty, &member_ty)?;

        if value_ty != member_ty{
            value = Expr::Cast(Box::new(value), member_ty.clone());
        }
        
        if let Expr::Member { object, key, optional } = member_expr{
            if optional{
                return Err(Error::syntax_error(member.span(), "invalid left-hand side assignment"))
            }

            self.check_assign_op_type(span, op, &member_ty)?;

            return Ok((Expr::MemberAssign { 
                op: op.into(), 
                object: object, 
                key: key, 
                value: Box::new(value) 
            }, member_ty))
        } else{
            unreachable!()
        }
    }

    pub fn translate_object_pat_assign(&mut self, pat: &swc::ObjectPat, op: swc::AssignOp, value: Expr) -> Result<(Expr, Type)>{
        todo!()
    }

    pub fn translate_array_pat_assign(&mut self, pat: &swc::ArrayPat, op: swc::AssignOp, value: Expr) -> Result<(Expr, Type)>{
        todo!()
    }

    pub fn translate_bin(&mut self, b: &swc::BinExpr) -> Result<(Expr, Type)> {
        if b.op == swc::BinaryOp::In {
            let prop = self.translate_computed_prop_name(&b.left)?;
            let (right, right_ty) = self.translate_expr(&b.right, None)?;

            match prop {
                PropNameOrExpr::PropName(prop) => {
                    if let Some(_) = self.type_has_property(&right_ty, &prop, false) {
                        return Ok((
                            Expr::Seq(Box::new(right), Box::new(Expr::Bool(true))),
                            Type::Bool,
                        ));
                    } else {
                        return Ok((
                            Expr::Seq(Box::new(right), Box::new(Expr::Bool(false))),
                            Type::Bool,
                        ));
                    }
                }
                PropNameOrExpr::Expr(..) => {
                    // TODO
                    return Err(Error::syntax_error(
                        b.span,
                        "computed property name 'in' operation not supported",
                    ));
                }
            }
        }
        let (mut left, mut left_ty) = self.translate_expr(&b.left, None)?;
        let (mut right, mut right_ty) = self.translate_expr(&b.right, None)?;

        let ty;

        match b.op {
            swc::BinaryOp::Add
            | swc::BinaryOp::Sub
            | swc::BinaryOp::Div
            | swc::BinaryOp::Mul
            | swc::BinaryOp::Mod
            | swc::BinaryOp::Exp => {
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    left_ty = Type::Number;
                    left = Expr::Cast(Box::new(left), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    right_ty = Type::Number;
                    right = Expr::Cast(Box::new(right), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                } else if b.op == swc::BinaryOp::Add
                    && left_ty == Type::String
                    && right_ty == Type::String
                {
                    ty = Type::String
                } else {
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                debug_assert!(right_ty == left_ty);
            }
            swc::BinaryOp::BitAnd
            | swc::BinaryOp::BitOr
            | swc::BinaryOp::BitXor
            | swc::BinaryOp::LShift
            | swc::BinaryOp::RShift
            | swc::BinaryOp::ZeroFillRShift => {
                if left_ty == Type::Number {
                    left_ty = Type::Int;
                    left = Expr::Cast(Box::new(left), Type::Int);
                }
                if right_ty == Type::Number {
                    right_ty = Type::Int;
                    right = Expr::Cast(Box::new(right), Type::Int)
                }
                if !(left_ty == Type::Int && right_ty == Type::Int)
                    || !(left_ty == Type::Bigint && right_ty == Type::Bigint)
                {
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                debug_assert!(left_ty == right_ty);

                ty = Type::Int;
            }
            swc::BinaryOp::EqEq
            | swc::BinaryOp::EqEqEq
            | swc::BinaryOp::NotEq
            | swc::BinaryOp::NotEqEq => {
                ty = Type::Bool;
            }
            swc::BinaryOp::Gt | swc::BinaryOp::GtEq | swc::BinaryOp::Lt | swc::BinaryOp::LtEq => {
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    left_ty = Type::Number;
                    left = Expr::Cast(Box::new(left), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    right_ty = Type::Number;
                    right = Expr::Cast(Box::new(right), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                } else {
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                debug_assert!(right_ty == left_ty);
            }
            swc::BinaryOp::NullishCoalescing | swc::BinaryOp::LogicalOr => {
                ty = left_ty.union(right_ty);
            }
            swc::BinaryOp::LogicalAnd => {
                if left_ty != Type::Bool {
                    left_ty = Type::Bool;
                    left = Expr::Cast(Box::new(left), Type::Bool);
                }
                if right_ty != Type::Bool {
                    right_ty = Type::Bool;
                    right = Expr::Cast(Box::new(right), Type::Bool);
                }

                debug_assert_eq!(right_ty, left_ty);
                debug_assert_eq!(right_ty, Type::Bool);

                ty = Type::Bool
            }
            swc::BinaryOp::In | swc::BinaryOp::InstanceOf => unreachable!(),
        };

        return Ok((
            Expr::Bin {
                op: b.op.into(),
                left: Box::new(left),
                right: Box::new(right),
            },
            ty,
        ));
    }

    /// translates the call expression.
    /// currently, generics are not supported yet
    pub fn translate_call(&mut self, call: &swc::CallExpr) -> Result<(Expr, Type)> {
        let (callee, callee_ty) = match &call.callee {
            swc::Callee::Super(s) => {
                if !self.is_in_constructor {
                    return Err(Error::syntax_error(
                        s.span,
                        "super call is only allowed in constructors",
                    ));
                }

                let sup = self.super_class.expect("invalid super class");

                let constructor = &self
                    .context
                    .classes
                    .get(&sup)
                    .expect("invalid class")
                    .constructor;

                let func_ty = if let Some((_, ty)) = constructor {
                    ty.clone()
                } else {
                    FuncType {
                        this_ty: Type::Object(sup),
                        params: Vec::new(),
                        var_arg: false,
                        return_ty: Type::Undefined,
                    }
                };

                (Callee::Super(sup), func_ty)
            }
            swc::Callee::Import(i) => {
                return Err(Error::syntax_error(i.span, "dynamic import not allowed"))
            }
            swc::Callee::Expr(e) => {
                // translate the expression
                let (expr, ty) = self.translate_expr(e, None)?;

                // check it is a function
                let func_ty = if let Type::Function(func) = ty {
                    *func
                } else {
                    return Err(Error::syntax_error(call.span, "callee is not a function"));
                };

                // convert to callee
                match expr {
                    Expr::Member {
                        object,
                        key,
                        optional: _,
                    } => {
                        // check if object matches func_ty.this type
                        // TODO
                        (
                            Callee::Member {
                                object: *object,
                                prop: key,
                            },
                            func_ty,
                        )
                    }
                    Expr::Function(f) => {
                        // check this type matches
                        let this_ty = self.this_ty.clone();
                        self.type_check(call.span, &this_ty, &func_ty.this_ty)?;

                        (Callee::Function(f), func_ty)
                    }
                    _ => {
                        // check this type matches
                        let this_ty = self.this_ty.clone();
                        self.type_check(call.span, &this_ty, &func_ty.this_ty)?;

                        (Callee::Expr(expr), func_ty)
                    }
                }
            }
        };

        // type arguments
        if call.type_args.is_some() {
            todo!("generics")
        }

        // if it is not a member call, we have to check
        if !callee.is_member() {
            let this_ty = self.this_ty.clone();
            self.type_check(call.span, &this_ty, &callee_ty.this_ty)?;
        }

        // reference argument types
        let expected_arguments: &[Type] = &callee_ty.params;

        let mut args = Vec::new();

        // since var args is not supported, length of arguments is fixed
        if call.args.len() != callee_ty.params.len() {
            return Err(Error::syntax_error(
                call.span,
                format!(
                    "expected {} arguments, {} were given",
                    callee_ty.params.len(),
                    call.args.len()
                ),
            ));
        }

        // handle arguments
        for (i, arg) in call.args.iter().enumerate() {
            // spread ... is present
            if let Some(spread) = arg.spread {
                return Err(Error::syntax_error(
                    spread,
                    "variable arguments not supported",
                ));
            }

            // translate argument
            let (a, arg_ty) = self.translate_expr(&arg.expr, expected_arguments.get(i))?;

            // check argument fulfills type
            self.type_check(arg.span(), &arg_ty, &expected_arguments[i])?;

            if arg_ty.eq(&expected_arguments[i]) {
                // push expression to arguments
                args.push(a);
            } else {
                // convert value to type
                args.push(Expr::Cast(Box::new(a), expected_arguments[i].clone()))
            }
        }

        return Ok((
            Expr::Call {
                callee: Box::new(callee),
                args: args,
                optional: false,
            },
            callee_ty.return_ty,
        ));
    }

    pub fn translate_cond(&mut self, cond: &swc::CondExpr) -> Result<(Expr, Type)> {
        let (mut test, test_ty) = self.translate_expr(&cond.test, None)?;
        let (cons, cons_ty) = self.translate_expr(&cond.cons, None)?;
        let (alt, alt_ty) = self.translate_expr(&cond.alt, None)?;

        if test_ty != Type::Bool {
            // cast it to bool
            test = Expr::Cast(Box::new(test), Type::Bool);
        }

        return Ok((
            Expr::Ternary {
                test: Box::new(test),
                left: Box::new(cons),
                right: Box::new(alt),
            },
            cons_ty.union(alt_ty),
        ));
    }

    pub fn translate_var_load(&mut self, ident: &swc::Ident) -> Result<(Expr, Type)> {
        match self.context.find(&ident.sym) {
            Some(Binding::Var {
                writable,
                redeclarable: _,
                id,
                ty,
            }) => {
                if !*writable {
                    return Err(Error::syntax_error(
                        ident.span,
                        "constant is not assignable",
                    ));
                }

                return Ok((
                    Expr::VarLoad {
                        span: ident.span,
                        variable: *id,
                    },
                    ty.clone(),
                ));
            }
            Some(Binding::Using { id, ty, .. }) => {
                return Ok((
                    Expr::VarLoad {
                        span: ident.span,
                        variable: *id,
                    },
                    ty.clone(),
                ))
            }
            None => {
                return Err(Error::syntax_error(
                    ident.span,
                    format!("undefined identifier '{}'", ident.sym),
                ))
            }
            _ => return Err(Error::syntax_error(ident.span, "binding is not a variable")),
        }
    }

    pub fn translate_member(&mut self, member: &swc::MemberExpr) -> Result<(Expr, Type)> {
        let (obj, obj_ty) = self.translate_expr(&member.obj, None)?;

        let prop = match &member.prop {
            swc::MemberProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::MemberProp::Ident(id) => {
                PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
            }
            swc::MemberProp::PrivateName(id) => {
                if self.this_ty == obj_ty {
                    PropNameOrExpr::PropName(PropName::Private(id.id.sym.to_string()))
                } else {
                    return Err(Error::syntax_error(
                        id.span,
                        "cannot access privite properties outside of method",
                    ));
                }
            }
        };

        match prop {
            PropNameOrExpr::PropName(name) => {
                if let Some(member_ty) = self.type_has_property(&obj_ty, &name, false) {
                    return Ok((
                        Expr::Member {
                            object: Box::new(obj),
                            key: PropNameOrExpr::PropName(name),
                            optional: false,
                        },
                        member_ty,
                    ));
                } else {
                    return Err(Error::syntax_error(
                        member.span,
                        format!("type has no property '{}'", name),
                    ));
                }
            }
            PropNameOrExpr::Expr(mut e, ty) => {
                match &obj_ty {
                    Type::Map(k, v) => {
                        self.type_check(member.span, &ty, k)?;

                        if &ty != k.as_ref() {
                            e = Box::new(Expr::Cast(e, k.as_ref().clone()));
                        }
                        return Ok((
                            Expr::Member {
                                object: Box::new(obj),
                                key: PropNameOrExpr::Expr(e, ty),
                                optional: false,
                            },
                            v.as_ref().clone(),
                        ));
                    }
                    Type::Array(elem) => {
                        if ty == Type::Int || ty == Type::Number {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                elem.as_ref().clone(),
                            ));
                        } else {
                            return Err(Error::syntax_error(
                                member.span,
                                "array can only be indexed by number",
                            ));
                        }
                    }
                    Type::Tuple(elems) => {
                        if ty == Type::Int || ty == Type::Number {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                Type::Union(elems.clone()),
                            ));
                        } else {
                            return Err(Error::syntax_error(
                                member.span,
                                "tuple can only be indexed by number",
                            ));
                        }
                    }
                    _ => {
                        return Err(Error::syntax_error(
                            member.span,
                            "type '' is not indexable, property must be literal",
                        ))
                    }
                };
            }
        }
    }

    pub fn translate_unary_expr(&mut self, u: &swc::UnaryExpr) -> Result<(Expr, Type)> {
        let (mut expr, ty) = self.translate_expr(&u.arg, None)?;
        match u.op {
            swc::UnaryOp::Bang => {
                if ty != Type::Bool {
                    // cast type to bool
                    expr = Expr::Cast(Box::new(expr), Type::Bool);
                }

                return Ok((
                    Expr::Unary {
                        op: crate::ast::UnaryOp::LogicalNot,
                        value: Box::new(expr),
                    },
                    Type::Bool,
                ));
            }
            swc::UnaryOp::Delete => {
                return Err(Error::syntax_error(u.span, "'delete' is not allowed"))
            }
            swc::UnaryOp::Minus | swc::UnaryOp::Plus => {
                if ty == Type::Number || ty == Type::Int || ty == Type::Bigint || ty == Type::Bool {
                    // return unary expression
                    return Ok((
                        Expr::Unary {
                            op: if u.op == swc::UnaryOp::Minus {
                                crate::ast::UnaryOp::Minus
                            } else {
                                crate::ast::UnaryOp::Plus
                            },
                            value: Box::new(expr),
                        },
                        match ty {
                            Type::Int => Type::Int,
                            Type::Bigint => Type::Bigint,
                            _ => Type::Number,
                        },
                    ));
                } else {
                    return Err(Error::syntax_error(
                        u.span,
                        "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                    ));
                }
            }
            swc::UnaryOp::Tilde => {
                // bitnot can only apply to number, bigint or bool
                if ty != Type::Number && ty != Type::Int && ty != Type::Bigint && ty != Type::Bool {
                    return Err(Error::syntax_error(
                        u.span,
                        "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                    ));
                }

                // convert to int if not
                if ty != Type::Int {
                    // cast type to int
                    expr = Expr::Cast(Box::new(expr), Type::Int);
                };

                // return expression
                return Ok((
                    Expr::Unary {
                        op: crate::ast::UnaryOp::BitNot,
                        value: Box::new(expr),
                    },
                    Type::Int,
                ));
            }
            swc::UnaryOp::TypeOf => {
                let ty_s = match ty {
                    Type::AnyObject
                    | Type::Null
                    | Type::Object(_)
                    | Type::Regex
                    | Type::Array(_)
                    | Type::Function(_)
                    | Type::Map(_, _)
                    | Type::Promise(_)
                    | Type::Tuple(_)
                    | Type::Iterator(_) => "object",
                    Type::Bigint => "bigint",
                    Type::Bool => "boolean",
                    Type::Enum(_) | Type::Int | Type::Number => "number",
                    Type::String => "string",
                    Type::Symbol => "symbol",
                    Type::Undefined => "undefined",
                    // these type cannot be known at compile time
                    Type::Any
                    | Type::Alias(_)
                    | Type::Generic(_)
                    | Type::Interface(_)
                    | Type::Union(_) => {
                        // runtime reflect
                        return Ok((
                            Expr::Unary {
                                op: crate::ast::UnaryOp::Typeof,
                                value: Box::new(expr),
                            },
                            Type::String,
                        ));
                    }
                };

                // return the literal string of type
                return Ok((Expr::String(ty_s.to_string()), Type::String));
            }
            swc::UnaryOp::Void => {
                // simply return undefined
                return Ok((
                    Expr::Unary {
                        op: crate::ast::UnaryOp::Void,
                        value: Box::new(expr),
                    },
                    Type::Undefined,
                ));
            }
        }
    }

    pub fn translate_update_expr(&mut self, u: &swc::UpdateExpr) -> Result<(Expr, Type)> {
        let (expr, ty) = self.translate_expr(&u.arg, None)?;

        let op = match u.op {
            swc::UpdateOp::MinusMinus => {
                if u.prefix {
                    crate::ast::UpdateOp::PrefixSub
                } else {
                    crate::ast::UpdateOp::SuffixSub
                }
            }
            swc::UpdateOp::PlusPlus => {
                if u.prefix {
                    crate::ast::UpdateOp::PrefixAdd
                } else {
                    crate::ast::UpdateOp::SuffixAdd
                }
            }
        };

        if ty != Type::Int && ty != Type::Number && ty != Type::Bigint {
            return Err(Error::syntax_error(
                u.span,
                "operand must have type 'number' or 'bigint'",
            ));
        }

        match expr {
            Expr::Member {
                object,
                key,
                optional,
            } => {
                if optional {
                    return Err(Error::syntax_error(
                        u.span,
                        "invalid left-hand side assignment",
                    ));
                }

                return Ok((
                    Expr::MemberUpdate {
                        op: op,
                        object: object,
                        key: key,
                    },
                    ty,
                ));
            }
            Expr::VarLoad { variable, span: _ } => {
                return Ok((
                    Expr::VarUpdate {
                        op: op,
                        variable: variable,
                    },
                    ty,
                ))
            }
            _ => {
                return Err(Error::syntax_error(
                    u.span,
                    "invalid left-hand side assignment",
                ))
            }
        };
    }

    pub fn translate_super_prop_expr(&mut self, s: &swc::SuperPropExpr) -> Result<(Expr, Type)> {
        if self.super_class.is_none() {
            return Err(Error::syntax_error(
                s.span,
                "'super' keyword unexpected here",
            ));
        }

        let prop = match &s.prop {
            swc::SuperProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::SuperProp::Ident(id) => {
                PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
            }
        };

        let prop = match prop {
            PropNameOrExpr::PropName(p) => p,
            PropNameOrExpr::Expr(..) => {
                return Err(Error::syntax_error(
                    s.span,
                    "super property must be literal",
                ))
            }
        };

        let super_class = self.super_class.unwrap();

        // if in constructor, super means the class itself
        if self.is_in_constructor {
            // get the class
            if let Some(cl) = self.context.classes.get(&super_class) {
                // find static property
                if let Some((vid, ty)) = cl.static_properties.get(&prop) {
                    return Ok((
                        Expr::VarLoad {
                            span: s.span,
                            variable: *vid,
                        },
                        ty.clone(),
                    ));
                }
                // find static functions
                if let Some((fid, ty)) = cl.static_methods.get(&prop) {
                    return Ok((Expr::Function(*fid), Type::Function(Box::new(ty.clone()))));
                }

                if let Some(_) = cl.static_generic_methods.get(&prop) {
                    return Err(Error::syntax_error(s.span, "missing type arguments"));
                }
            } else {
                // the class should be defined
                unreachable!()
            }

            // the super class has no static property
            return Err(Error::syntax_error(
                s.span,
                format!("super has no property '{}'", prop),
            ));
        }

        // context is in method
        if let Some(ty) = self.type_has_property(&Type::Object(super_class), &prop, false) {
            // return member expression
            return Ok((
                Expr::Member {
                    object: Box::new(
                        // cast this to super
                        Expr::Cast(Box::new(Expr::This), Type::Object(super_class)),
                    ),
                    key: PropNameOrExpr::PropName(prop),
                    optional: false,
                },
                ty,
            ));
        }

        return Err(Error::syntax_error(
            s.span,
            format!("super has no property '{}'", prop),
        ));
    }

    pub fn translate_new_expr(&mut self, n: &swc::NewExpr) -> Result<(Expr, Type)> {
        let ident = match n.callee.as_ident() {
            Some(ident) => ident,
            None => return Err(Error::syntax_error(n.callee.span(), "expected identifier")),
        };

        let mut arguments = Vec::new();

        match self.context.find(&ident.sym) {
            Some(Binding::Class(class_id)) => {
                // should have no type arguments
                if let Some(args) = &n.type_args {
                    return Err(Error::syntax_error(args.span, "expected 0 type arguments"));
                }

                let class_id = *class_id;

                let c = self.context.classes.get(&class_id).expect("invalid class");

                if let Some((_const_id, const_ty)) = &c.constructor {
                    let params = const_ty.params.clone();

                    if let Some(args) = &n.args {
                        if args.len() != const_ty.params.len() {
                            return Err(Error::syntax_error(
                                n.span,
                                format!(
                                    "expected {} arguments, {} were given",
                                    const_ty.params.len(),
                                    args.len()
                                ),
                            ));
                        }

                        for (i, arg) in args.iter().enumerate() {
                            if let Some(spread) = arg.spread {
                                return Err(Error::syntax_error(
                                    spread,
                                    "spread argument is not supported",
                                ));
                            }

                            let (arg, _) = self.translate_expr(&arg.expr, params.get(i))?;
                            arguments.push(arg);
                        }
                    } else {
                        if const_ty.params.len() != 0 {
                            return Err(Error::syntax_error(
                                n.span,
                                format!("expected {} arguments", const_ty.params.len()),
                            ));
                        }
                    };
                } else {
                    if n.args.as_ref().is_some_and(|a| a.len() != 0) {
                        return Err(Error::syntax_error(n.span, "expected 0 arguments"));
                    }
                };

                return Ok((
                    Expr::New {
                        class: class_id,
                        args: arguments,
                    },
                    Type::Object(class_id),
                ));
            }
            Some(Binding::GenericClass(_class_id)) => {
                todo!("generic class")
            }
            _ => return Err(Error::syntax_error(ident.span, "expected class")),
        };
    }

    pub fn translate_seq_expr(
        &mut self,
        s: &swc::SeqExpr,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        if let Some(first) = s.exprs.get(0) {
            let first = self.translate_expr(
                &first,
                if s.exprs.len() == 1 {
                    expected_ty
                } else {
                    None
                },
            )?;

            if let Some(second) = s.exprs.get(1) {
                let second = self.translate_expr(
                    &second,
                    if s.exprs.len() == 2 {
                        expected_ty
                    } else {
                        None
                    },
                )?;

                let mut expr = Expr::Seq(Box::new(first.0), Box::new(second.0));
                let mut ty = second.1;

                let mut i = 2;
                while let Some(e) = s.exprs.get(i) {
                    i += 1;
                    let next = self.translate_expr(
                        e,
                        if i == s.exprs.len() {
                            expected_ty
                        } else {
                            None
                        },
                    )?;

                    expr = Expr::Seq(Box::new(expr), Box::new(next.0));
                    ty = next.1;
                }

                return Ok((expr, ty));
            } else {
                return Ok(first);
            }
        };

        return Ok((Expr::Undefined, Type::Undefined));
    }

    pub fn translate_lit(
        &mut self,
        lit: &swc::Lit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match lit {
            swc::Lit::BigInt(b) => Ok((
                Expr::Bigint(b.value.to_i128().expect("i128 overflow")),
                Type::Bigint,
            )),
            swc::Lit::Bool(b) => Ok((Expr::Bool(b.value), Type::Bool)),
            swc::Lit::JSXText(_) => unimplemented!(),
            swc::Lit::Null(_) => Ok((Expr::Null, Type::Null)),
            swc::Lit::Num(n) => {
                if expected_ty == Some(&Type::Number) {
                    return Ok((Expr::Number(n.value), Type::Number));
                }
                if n.value.is_finite() && n.value as i32 as f64 == n.value {
                    return Ok((Expr::Int(n.value as i32), Type::Int));
                }

                return Ok((Expr::Number(n.value), Type::Number));
            }
            swc::Lit::Str(s) => Ok((Expr::String(s.value.to_string()), Type::String)),
            swc::Lit::Regex(_r) => Ok((Expr::Regex(), Type::Regex)),
        }
    }

    pub fn translate_optchain(&mut self, n: &swc::OptChainExpr) -> Result<(Expr, Type)> {
        match n.base.as_ref() {
            swc::OptChainBase::Member(m) => {
                let (mut expr, ty) = self.translate_member(m)?;
                if !n.optional {
                    return Ok((expr, ty));
                }

                // set optional to true
                if let Expr::Member { optional, .. } = &mut expr {
                    *optional = true;
                } else {
                    unreachable!()
                }

                return Ok((expr, ty.union(Type::Undefined)));
            }
            swc::OptChainBase::Call(c) => {
                let mut callee = None;
                let mut func_ty = None;

                // function call
                if let Some(ident) = c.callee.as_ident() {
                    match self.context.find(&ident.sym) {
                        Some(Binding::Function(f)) => {
                            if let Some(args) = &c.type_args {
                                return Err(Error::syntax_error(
                                    args.span,
                                    "expected 0 type arguments",
                                ));
                            }

                            let id = *f;
                            let ty = self
                                .context
                                .functions
                                .get(&id)
                                .expect("invalid function")
                                .ty();

                            self.type_check(ident.span, &self.this_ty, &ty.this_ty)?;

                            callee = Some(Callee::Function(id));
                            func_ty = Some(ty);
                        }
                        Some(Binding::GenericFunction(_id)) => {
                            todo!("generic function")
                        }
                        _ => {}
                    };
                }

                // expression
                if callee.is_none() {
                    let (expr, ty) = self.translate_expr(&c.callee, None)?;

                    let func = match &ty {
                        Type::Function(func_ty) => func_ty.as_ref().clone(),
                        Type::Union(u) => {
                            let mut func = None;

                            for ty in u.iter() {
                                match ty {
                                    Type::Null | Type::Undefined => {}
                                    Type::Function(f) => {
                                        if func.is_some() {
                                            return Err(Error::syntax_error(
                                                c.callee.span(),
                                                "type '' is not callable",
                                            ));
                                        }
                                        func = Some(f.as_ref().clone());
                                    }
                                    _ => {
                                        return Err(Error::syntax_error(
                                            c.callee.span(),
                                            "type '' is not callable",
                                        ))
                                    }
                                }
                            }

                            func.unwrap()
                        }
                        Type::Undefined => {
                            // will never call
                            return Ok((expr, Type::Undefined));
                        }
                        Type::Null => {
                            // will never call
                            return Ok((expr, Type::Null));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                c.callee.span(),
                                "type '' is not callable",
                            ))
                        }
                    };

                    // check this type matches
                    self.type_check(c.span, &self.this_ty, &func.this_ty)?;

                    // member call
                    if let Expr::Member {
                        object,
                        key,
                        optional,
                    } = expr
                    {
                        if optional {
                            return Err(Error::syntax_error(c.span, "callee cannot be optional"));
                        }
                        callee = Some(Callee::Member {
                            object: *object,
                            prop: key,
                        });
                    } else {
                        callee = Some(Callee::Expr(expr));
                    }
                    func_ty = Some(func);
                };

                let callee = callee.unwrap();
                let func_ty = func_ty.unwrap();

                let mut args = Vec::new();
                let mut arg_tys = Vec::new();

                // translate arguments
                for (i, arg) in c.args.iter().enumerate() {
                    if let Some(spread) = arg.spread {
                        return Err(Error::syntax_error(
                            spread,
                            "variabl arguments not supported",
                        ));
                    }
                    let (expr, ty) = self.translate_expr(&arg.expr, func_ty.params.get(i))?;
                    args.push(expr);
                    arg_tys.push(ty);
                }

                // length must be the same
                if args.len() != func_ty.params.len() {
                    return Err(Error::syntax_error(
                        c.span,
                        format!(
                            "expected {} arguments, {} were given",
                            func_ty.params.len(),
                            args.len()
                        ),
                    ));
                }

                // return call expression
                return Ok((
                    Expr::Call {
                        callee: Box::new(callee),
                        args: args,
                        optional: true,
                    },
                    func_ty.return_ty,
                ));
            }
        }
    }

    pub fn translate_prop_name(&mut self, name: &swc::PropName) -> Result<PropNameOrExpr> {
        match name {
            swc::PropName::BigInt(b) => Ok(PropNameOrExpr::PropName(crate::PropName::String(
                b.value.to_string(),
            ))),
            swc::PropName::Ident(id) => Ok(PropNameOrExpr::PropName(crate::PropName::Ident(
                id.sym.to_string(),
            ))),
            swc::PropName::Num(n) => {
                if n.value as i32 as f64 == n.value {
                    Ok(PropNameOrExpr::PropName(crate::PropName::Int(
                        n.value as i32,
                    )))
                } else {
                    Ok(PropNameOrExpr::PropName(crate::PropName::String(
                        n.value.to_string(),
                    )))
                }
            }
            swc::PropName::Str(s) => Ok(PropNameOrExpr::PropName(crate::PropName::String(
                s.value.to_string(),
            ))),
            swc::PropName::Computed(c) => self.translate_computed_prop_name(&c.expr),
        }
    }

    pub fn translate_computed_prop_name(&mut self, expr: &swc::Expr) -> Result<PropNameOrExpr> {
        match expr {
            swc::Expr::PrivateName(n) => {
                return Ok(PropNameOrExpr::PropName(PropName::Private(
                    n.id.sym.to_string(),
                )))
            }
            swc::Expr::Lit(l) => {
                match l {
                    swc::Lit::BigInt(b) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            b.value.to_string(),
                        )))
                    }
                    swc::Lit::Bool(b) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            b.value.to_string(),
                        )))
                    }
                    swc::Lit::JSXText(j) => {
                        return Err(Error::syntax_error(j.span, "JSXText is not allowed"))
                    }
                    swc::Lit::Null(_) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            "null".to_string(),
                        )))
                    }
                    swc::Lit::Num(n) => {
                        if n.value as i32 as f64 == n.value {
                            return Ok(PropNameOrExpr::PropName(crate::PropName::Int(
                                n.value as i32,
                            )));
                        } else {
                            return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                                n.value.to_string(),
                            )));
                        };
                    }
                    swc::Lit::Regex(r) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(format!(
                            "/{}/{}",
                            r.exp, r.flags
                        ))))
                    }
                    swc::Lit::Str(s) => {
                        return Ok(PropNameOrExpr::PropName(crate::PropName::String(
                            s.value.to_string(),
                        )))
                    }
                };
            }
            swc::Expr::Member(mem) => match mem.obj.as_ref() {
                swc::Expr::Ident(id) => {
                    if id.sym.as_ref() == "Symbol" {
                        if let Some(prop) = mem.prop.as_ident() {
                            match prop.sym.as_ref() {
                                "asyncIterator" => {
                                    return Ok(PropNameOrExpr::PropName(crate::PropName::Symbol(
                                        crate::Symbol::AsyncIterator,
                                    )))
                                }
                                "hasInstance" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::HasInstance,
                                    )))
                                }
                                "isConcatSpreadable" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::IsConcatSpreadable,
                                    )))
                                }
                                "iterator" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Iterator,
                                    )))
                                }
                                "match" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Match,
                                    )))
                                }
                                "matchAll" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::MatchAll,
                                    )))
                                }
                                "replace" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Replace,
                                    )))
                                }
                                "search" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Search,
                                    )))
                                }
                                "species" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Species,
                                    )))
                                }
                                "split" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Split,
                                    )))
                                }
                                "toStringTag" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::ToStringTag,
                                    )))
                                }
                                "unscopables" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Unscopables,
                                    )))
                                }
                                "dispose" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::Dispose,
                                    )))
                                }
                                "asyncDispose" => {
                                    return Ok(PropNameOrExpr::PropName(PropName::Symbol(
                                        crate::Symbol::AsyncDispose,
                                    )))
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        };

        let (expr, ty) = self.translate_expr(expr, None)?;

        return Ok(PropNameOrExpr::Expr(Box::new(expr), ty))
    }
}
