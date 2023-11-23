use native_js_common::error::Error;
use swc_common::{Span, Spanned};
use swc_ecmascript::ast as swc;

use crate::{
    ast::{Expr, Type, Callee, FuncType},
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId}, PropName,
};

use super::{Transformer, context::Binding};

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
                todo!()
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
                return Err(Error::syntax_error(c.span(), "class expression not allowed"))
            }
            swc::Expr::Cond(cond) => self.translate_cond(cond)?,
            swc::Expr::Fn(f) => {
                // generate a new function id
                let id = FunctionId::new();
                self.translate_function(id, &f.function)?;

                let ty = self.context.functions.get(&id).expect("invalid function").ty();

                (Expr::Function(id), Type::Function(Box::new(ty)))
            },
            swc::Expr::Ident(id) => self.translate_var_load(id)?,
            swc::Expr::This(_) => {
                (Expr::This, self.this_ty.clone())
            },
            swc::Expr::Object(o) => {
                return Err(Error::syntax_error(o.span, "object literal not allowed"))
            },
            swc::Expr::Unary(u) => self.translate_unary_expr(u)?,
            swc::Expr::Update(u) => self.translate_update_expr(u)?,
            swc::Expr::Member(m) => self.translate_member(m)?,
            swc::Expr::SuperProp(s) => self.translate_super_prop_expr(s)?,
        };

        if let Some(expected) = expected_ty {
            self.type_check(expr.span(), &ty, expected)?;

            if !ty.eq(expected) {
                e = Expr::Cast(Box::new(e), expected.clone())
            }
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

                    let (span, expr, ty) = if let Some(elem) = elem {
                        if let Some(spread) = elem.spread {
                            return Err(Error::syntax_error(
                                spread,
                                "spread expression not allowed",
                            ));
                        }
                        let (e, t) = self.translate_expr(&elem.expr, Some(expected))?;

                        (elem.span(), e, t)
                    } else {
                        (a.span, Expr::Undefined, Type::Undefined)
                    };

                    self.type_check(span, &ty, expected)?;

                    values.push(expr);
                }

                return Ok((
                    Expr::Array { values: values },
                    Type::Tuple(element_tys.clone()),
                ));
            }
            _ => {
                let mut ty:Option<Type> = None;
                let mut values = Vec::new();

                for elem in &a.elems {

                    // translate the element if not empty
                    let (expr, t) = if let Some(elem) = elem {
                        let (expr, t) = self.translate_expr(&elem.expr, None)?;
                        (expr, t)
                    } else{
                        // if it is empty, it is undefined
                        (Expr::Undefined, Type::Undefined)
                    };

                    if let Some(chained) = ty{
                        ty = Some(chained.union(t));
                    } else{
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
        let (member_or_var, ty) = self.translate_member_or_var(&a.left)?;

        let (mut value, value_ty) = self.translate_expr(&a.right, Some(&ty))?;

        // check if value satisfies variable type
        self.type_check(a.span, &value_ty, &ty);

        if ty != value_ty{
            value = Expr::Cast(Box::new(value), ty.clone());
        }

        match member_or_var {
            Expr::Member { object, key } => {
                return Ok((
                    Expr::MemberAssign {
                        op: a.op.into(),
                        object: object,
                        key: key,
                        value: Box::new(value),
                    },
                    ty,
                ))
            },
            Expr::VarLoad { variable } => {
                return Ok((
                    Expr::VarAssign { 
                        op: a.op.into(), 
                        variable: variable, 
                        value: Box::new(value)
                    },
                    ty
                ))
            }
            _ => unreachable!()
        }
    }

    pub fn translate_bin(&mut self, b: &swc::BinExpr) -> Result<(Expr, Type)> {
        if b.op == swc::BinaryOp::In{
            let prop = self.translate_computed_prop_name(&b.left)?;
            let (right, right_ty) = self.translate_expr(&b.right, None)?;

            if let Some(_) = self.type_has_property(b.span, &right_ty, &prop){
                return Ok((
                    Expr::Seq(Box::new(right), Box::new(Expr::Bool(true))),
                    Type::Bool
                ))
            } else{
                return Ok((
                    Expr::Seq(Box::new(right), Box::new(Expr::Bool(false))),
                    Type::Bool
                ))
            }
        }
        let (mut left, mut left_ty) = self.translate_expr(&b.left, None)?;
        let (mut right, mut right_ty) = self.translate_expr(&b.right, None)?;

        let ty;

        match b.op{
            swc::BinaryOp::Add
            | swc::BinaryOp::Sub
            | swc::BinaryOp::Div
            | swc::BinaryOp::Mul
            | swc::BinaryOp::Mod 
            | swc::BinaryOp::Exp => {
                if left_ty == Type::Number && right_ty == Type::Number{
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int{
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number{
                    left_ty = Type::Number;
                    left = Expr::Cast(Box::new(left), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int{
                    right_ty = Type::Number;
                    right = Expr::Cast(Box::new(right), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint{
                    ty = Type::Bigint;
                } else if b.op == swc::BinaryOp::Add && left_ty == Type::String && right_ty == Type::String{
                    ty = Type::String
                } else{
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")))
                }

                debug_assert!(right_ty == left_ty);
            },
            swc::BinaryOp::BitAnd
            | swc::BinaryOp::BitOr
            | swc::BinaryOp::BitXor
            | swc::BinaryOp::LShift
            | swc::BinaryOp::RShift
            | swc::BinaryOp::ZeroFillRShift => {
                if left_ty == Type::Number{
                    left_ty = Type::Int;
                    left = Expr::Cast(Box::new(left), Type::Int);
                }
                if right_ty == Type::Number{
                    right_ty = Type::Int;
                    right = Expr::Cast(Box::new(right), Type::Int)
                }
                if !(left_ty == Type::Int && right_ty == Type::Int) || !(left_ty == Type::Bigint && right_ty == Type::Bigint){
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")))
                }

                debug_assert!(left_ty == right_ty);
                
                ty = Type::Int;
            },
            swc::BinaryOp::EqEq
            | swc::BinaryOp::EqEqEq 
            | swc::BinaryOp::NotEq
            | swc::BinaryOp::NotEqEq => {
                ty = Type::Bool;
            },
            swc::BinaryOp::Gt
            | swc::BinaryOp::GtEq
            | swc::BinaryOp::Lt
            | swc::BinaryOp::LtEq => {
                if left_ty == Type::Number && right_ty == Type::Number{
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int{
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number{
                    left_ty = Type::Number;
                    left = Expr::Cast(Box::new(left), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int{
                    right_ty = Type::Number;
                    right = Expr::Cast(Box::new(right), Type::Number);
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint{
                    ty = Type::Bigint;
                } else{
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")))
                }

                debug_assert!(right_ty == left_ty);
            }
            swc::BinaryOp::NullishCoalescing
            | swc::BinaryOp::LogicalOr => {
                ty = left_ty.union(right_ty);
            },
            swc::BinaryOp::LogicalAnd => {
                if left_ty != Type::Bool{
                    left_ty = Type::Bool;
                    left = Expr::Cast(Box::new(left), Type::Bool);
                }
                if right_ty != Type::Bool{
                    right_ty = Type::Bool;
                    right = Expr::Cast(Box::new(right), Type::Bool);
                }

                debug_assert_eq!(right_ty, left_ty);
                debug_assert_eq!(right_ty, Type::Bool);

                ty = Type::Bool
            },
            swc::BinaryOp::In
            | swc::BinaryOp::InstanceOf => unreachable!()
        };

        return Ok((
            Expr::Bin { 
                op: b.op.into(), 
                left: Box::new(left), 
                right: Box::new(right) 
            },
            ty
        ))
    }

    /// translates a member expression or a variable expression from pat or expr
    pub fn translate_member_or_var(&mut self, pat: &swc::PatOrExpr) -> Result<(Expr, Type)> {
        match pat {
            swc::PatOrExpr::Expr(e) => if let Some(member) = e.as_member() {
                return self.translate_member(member)
            } else if let Some(id) = e.as_ident(){
                return self.translate_var_load(id)
            },
            swc::PatOrExpr::Pat(p) => if let Some(ident) = p.as_ident() {
                return self.translate_var_load(&ident.id)
            },
        };

        return Err(Error::syntax_error(pat.span(), "invalid left-hand side assignment"));
    }

    /// translates the call expression.
    /// currently, generics are not supported yet
    pub fn translate_call(&mut self, call: &swc::CallExpr) -> Result<(Expr, Type)>{
        let (callee, callee_ty) =
        match &call.callee{
            swc::Callee::Super(s) => {
                if !self.is_in_constructor{
                    return Err(Error::syntax_error(s.span, "super call is only allowed in constructors"))
                }

                let sup = self.super_class.expect("invalid super class");

                let constructor = self.context.classes.get(&sup).expect("invalid class").constructor;

                let func_ty = if let Some(constructor) = constructor{
                    self.context.functions.get(&constructor).expect("invalid function").ty()
                } else{
                    FuncType{
                        this_ty: Type::Object(sup),
                        params: Vec::new(),
                        var_arg: false,
                        return_ty: Type::Undefined
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
                let func_ty = if let Type::Function(func) = ty{
                    *func
                } else{
                    return Err(Error::syntax_error(call.span, "callee is not a function"))
                };

                // convert to callee
                match expr{
                    Expr::Member { object, key } => {
                        // check if object matches this type
                        // todo!()
                        (Callee::Member(*object, key), func_ty)
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
        if call.type_args.is_some(){
            todo!("generics")
        }

        // if it is not a member call, we have to check 
        if !callee.is_member(){
            let this_ty = self.this_ty.clone();
            self.type_check(call.span, &this_ty, &callee_ty.this_ty)?;
        }

        // reference argument types
        let expected_arguments:&[Type] = &callee_ty.params;

        let mut args = Vec::new();

        // since var args is not supported, length of arguments is fixed
        if call.args.len() != callee_ty.params.len(){
            return Err(Error::syntax_error(call.span, format!("expected {} arguments, {} were given", callee_ty.params.len(), call.args.len())))
        }

        // handle arguments
        for (i, arg) in call.args.iter().enumerate(){
            // spread ... is present
            if let Some(spread) = arg.spread{
                return Err(Error::syntax_error(spread, "variable arguments not supported"))
            }

            // translate argument
            let (a, arg_ty) = self.translate_expr(&arg.expr, expected_arguments.get(i))?;

            // check argument fulfills type
            self.type_check(arg.span(), &arg_ty, &expected_arguments[i])?;

            if arg_ty.eq(&expected_arguments[i]){
                // push expression to arguments
                args.push(a);
            } else{
                // convert value to type
                args.push(Expr::Cast(Box::new(a), expected_arguments[i].clone()))
            }
        };

        return Ok((
            Expr::Call { callee: Box::new(callee), args: args, optchain: false },
            callee_ty.return_ty
        ))
    }

    pub fn translate_cond(&mut self, cond: &swc::CondExpr) -> Result<(Expr, Type)>{
        let (mut test, test_ty) = self.translate_expr(&cond.test, None)?;
        let (cons, cons_ty) = self.translate_expr(&cond.cons, None)?;
        let (alt, alt_ty) = self.translate_expr(&cond.alt, None)?;

        if test_ty != Type::Bool{
            // cast it to bool
            test = Expr::Cast(Box::new(test), Type::Bool);
        }

        return Ok((
            Expr::Ternary { 
                test: Box::new(test), 
                left: Box::new(cons), 
                right: Box::new(alt)
            },
            cons_ty.union(alt_ty)
        ))
    }

    pub fn translate_var_load(&mut self, ident: &swc::Ident) -> Result<(Expr, Type)>{
        match self.context.find(&ident.sym){
            Some(Binding::Var { writable, redeclarable:_, id, ty }) => {
                if !*writable{
                    return Err(Error::syntax_error(ident.span, "constant is not assignable"))
                }

                return Ok((
                    Expr::VarLoad { variable: *id },
                    ty.clone()
                ))
            }
            Some(Binding::Using { id, ty }) => {
                return Ok((
                    Expr::VarLoad { variable: *id },
                    ty.clone()
                ))
            }
            None => {
                return Err(Error::syntax_error(ident.span, "undefined identifier"))
            }
            _ => {
                return Err(Error::syntax_error(ident.span, "binding is not a variable"))
            }
        }
    }

    pub fn translate_member(&mut self, member: &swc::MemberExpr) -> Result<(Expr, Type)>{
        let (obj, obj_ty) = self.translate_expr(&member.obj, None)?;

        let prop = match &member.prop{
            swc::MemberProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::MemberProp::Ident(id) => PropName::Ident(id.sym.to_string()),
            swc::MemberProp::PrivateName(id) => {
                if self.this_ty == obj_ty{
                    PropName::Private(id.id.sym.to_string())
                } else{
                    return Err(Error::syntax_error(id.span, "cannot access privite properties outside of method"))
                }
            }
        };

        if let Some(member_ty) = self.type_has_property(member.span, &obj_ty, &prop){
            return Ok((
                Expr::Member { object: Box::new(obj), key: prop },
                member_ty
            ))
        } else{
            return Err(Error::syntax_error(member.span, format!("type has no property '{}'", prop)))
        }
    }

    pub fn translate_unary_expr(&mut self, u: &swc::UnaryExpr) -> Result<(Expr, Type)>{
        let (mut expr, mut ty) = self.translate_expr(&u.arg, None)?;
        match u.op{
            swc::UnaryOp::Bang => {
                if ty != Type::Bool{
                    // cast type to bool
                    expr = Expr::Cast(Box::new(expr), Type::Bool);
                    ty = Type::Bool;
                }

                return Ok((
                    Expr::Unary { op: crate::ast::UnaryOp::LogicalNot, value: Box::new(expr) },
                    Type::Bool
                ))
            },
            swc::UnaryOp::Delete => {
                return Err(Error::syntax_error(u.span, "'delete' is not allowed"))
            }
            swc::UnaryOp::Minus 
            | swc::UnaryOp::Plus => {
                if ty == Type::Number || ty == Type::Int || ty == Type::Bigint || ty == Type::Bool{
                    // return unary expression
                    return Ok((
                        Expr::Unary {
                            op: if u.op == swc::UnaryOp::Minus{
                                crate::ast::UnaryOp::Minus
                            } else{
                                crate::ast::UnaryOp::Plus
                            },
                            value: Box::new(expr) 
                        },
                        match ty{
                            Type::Int => Type::Int,
                            Type::Bigint => Type::Bigint,
                            _ => Type::Number
                        }
                    ))
                } else{
                    return Err(Error::syntax_error(u.span, "right-hand side must be one of 'number', 'bigint' or 'boolean'"))
                }
            },
            swc::UnaryOp::Tilde => {
                // bitnot can only apply to number, bigint or bool
                if ty != Type::Number && ty != Type::Int && ty != Type::Bigint && ty != Type::Bool{
                    return Err(Error::syntax_error(u.span, "right-hand side must be one of 'number', 'bigint' or 'boolean'"))
                }

                // convert to int if not
                if ty != Type::Int{
                    // cast type to int
                    expr = Expr::Cast(Box::new(expr), Type::Int);
                };

                // return expression
                return Ok((
                    Expr::Unary { 
                        op: crate::ast::UnaryOp::BitNot, 
                        value: Box::new(expr),
                    },
                    Type::Int
                ))
            },
            swc::UnaryOp::TypeOf => {
                let ty_s =
                match ty{
                    Type::AnyObject 
                    | Type::Null 
                    | Type::Object(_) 
                    | Type::Regex 
                    | Type::Array(_) 
                    | Type::Function(_)
                    | Type::Map(_, _)
                    | Type::Promise(_)
                    | Type::Tuple(_) => "object",
                    Type::Bigint => "bigint",
                    Type::Bool => "boolean",
                    Type::Enum(_)
                    | Type::Int
                    | Type::Number => "number",
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
                                value: Box::new(expr)
                            },
                            Type::String
                        ))
                    }
                };

                // return the literal string of type
                return Ok((
                    Expr::String(ty_s.to_string()),
                    Type::String
                ))
            }
            swc::UnaryOp::Void => {
                // simply return undefined
                return Ok((
                    Expr::Unary { 
                        op: crate::ast::UnaryOp::Void, 
                        value: Box::new(expr)
                    },
                    Type::Undefined
                ))
            }
        }
    }

    pub fn translate_update_expr(&mut self, u: &swc::UpdateExpr) -> Result<(Expr, Type)>{
        let (expr, ty) = self.translate_expr(&u.arg, None)?;

        let op = match u.op{
            swc::UpdateOp::MinusMinus => if u.prefix{
                crate::ast::UpdateOp::PrefixSub
            } else{
                crate::ast::UpdateOp::SuffixSub
            },
            swc::UpdateOp::PlusPlus => if u.prefix{
                crate::ast::UpdateOp::PrefixAdd
            } else{
                crate::ast::UpdateOp::SuffixAdd
            }
        };

        if ty != Type::Int && ty != Type::Number && ty != Type::Bigint{
            return Err(Error::syntax_error(u.span, "operand must have type 'number' or 'bigint'"))
        }

        match expr{
            Expr::Member { object, key } => {
                return Ok((
                    Expr::MemberUpdate { 
                        op: op, 
                        object: object, 
                        key: key 
                    },
                    ty
                ))
            },
            Expr::VarLoad { variable } => {
                return Ok((
                    Expr::VarUpdate { 
                        op: op, 
                        variable: variable
                    },
                    ty
                ))
            }
            _ => return Err(Error::syntax_error(u.span, "invalid left-hand side assignment"))
        };
    }

    pub fn translate_super_prop_expr(&mut self, s: &swc::SuperPropExpr) -> Result<(Expr, Type)>{
        if self.super_class.is_none(){
            return Err(Error::syntax_error(s.span, "'super' keyword unexpected here"))
        }

        let prop = match &s.prop{
            swc::SuperProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::SuperProp::Ident(id) => PropName::Ident(id.sym.to_string()),
        };

        let super_class = self.super_class.unwrap();

        // if in constructor, super means the class itself
        if self.is_in_constructor{
            // get the class
            if let Some(cl) = self.context.classes.get(&super_class){
                // find static property
                if let Some((vid, ty)) = cl.static_properties.get(&prop){
                    return Ok((
                        Expr::VarLoad { variable: *vid },
                        ty.clone()
                    ))
                }
                // find static functions
                if let Some((fid, ty)) = cl.static_methods.get(&prop){
                    return Ok((
                        Expr::Function(*fid),
                        Type::Function(Box::new(ty.clone()))
                    ))
                }

                if let Some(_) = cl.static_generic_methods.get(&prop){
                    return Err(Error::syntax_error(s.span, "missing type arguments"))
                }
            } else{
                // the class should be defined
                unreachable!()
            }

            // the super class has no static property
            return Err(Error::syntax_error(s.span, format!("super has no property '{}'", prop)))
        }

        // context is in method
        if let Some(ty) = self.type_has_property(s.span, &Type::Object(super_class), &prop){
            // return member expression
            return Ok((
                Expr::Member { 
                    object: Box::new(
                        // cast this to super
                        Expr::Cast(Box::new(Expr::This), Type::Object(super_class))
                    ), 
                    key: prop
                },
                ty
            ))
        }

        return Err(Error::syntax_error(s.span, format!("super has no property '{}'", prop)))
    }

    pub fn translate_new_expr(&mut self, n: &swc::NewExpr) -> Result<(Expr, Type)>{
        
    }

    pub fn translate_prop_name(&mut self, name: &swc::PropName) -> Result<crate::PropName> {
        match name {
            swc::PropName::BigInt(b) => Ok(crate::PropName::String(b.value.to_string())),
            swc::PropName::Ident(id) => Ok(crate::PropName::Ident(id.sym.to_string())),
            swc::PropName::Num(n) => {
                if n.value as i32 as f64 == n.value {
                    Ok(crate::PropName::Int(n.value as i32))
                } else {
                    Ok(crate::PropName::String(n.value.to_string()))
                }
            }
            swc::PropName::Str(s) => Ok(crate::PropName::String(s.value.to_string())),
            swc::PropName::Computed(c) => self.translate_computed_prop_name(&c.expr),
        }
    }

    fn translate_computed_prop_name(&self, expr: &swc::Expr) -> Result<crate::PropName> {
        match expr {
            swc::Expr::PrivateName(n) => return Ok(crate::PropName::Private(n.id.sym.to_string())),
            swc::Expr::Lit(l) => {
                match l {
                    swc::Lit::BigInt(b) => return Ok(crate::PropName::String(b.value.to_string())),
                    swc::Lit::Bool(b) => return Ok(crate::PropName::String(b.value.to_string())),
                    swc::Lit::JSXText(j) => {
                        return Err(Error::syntax_error(j.span, "JSXText is not allowed"))
                    }
                    swc::Lit::Null(_) => return Ok(crate::PropName::String("null".to_string())),
                    swc::Lit::Num(n) => {
                        if n.value as i32 as f64 == n.value {
                            return Ok(crate::PropName::Int(n.value as i32));
                        } else {
                            return Ok(crate::PropName::String(n.value.to_string()));
                        };
                    }
                    swc::Lit::Regex(r) => {
                        return Ok(crate::PropName::String(format!("/{}/{}", r.exp, r.flags)))
                    }
                    swc::Lit::Str(s) => return Ok(crate::PropName::String(s.value.to_string())),
                };
            }
            swc::Expr::Member(mem) => match mem.obj.as_ref() {
                swc::Expr::Ident(id) => {
                    if id.sym.as_ref() == "Symbol" {
                        if let Some(prop) = mem.prop.as_ident() {
                            match prop.sym.as_ref() {
                                "asyncIterator" => {
                                    return Ok(crate::PropName::Symbol(
                                        crate::Symbol::AsyncIterator,
                                    ))
                                }
                                "hasInstance" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::HasInstance))
                                }
                                "isConcatSpreadable" => {
                                    return Ok(crate::PropName::Symbol(
                                        crate::Symbol::IsConcatSpreadable,
                                    ))
                                }
                                "iterator" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Iterator))
                                }
                                "match" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Match))
                                }
                                "matchAll" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::MatchAll))
                                }
                                "replace" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Replace))
                                }
                                "search" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Search))
                                }
                                "species" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Species))
                                }
                                "split" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Split))
                                }
                                "toStringTag" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::ToStringTag))
                                }
                                "unscopables" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Unscopables))
                                }
                                "dispose" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::Dispose))
                                }
                                "asyncDispose" => {
                                    return Ok(crate::PropName::Symbol(crate::Symbol::AsyncDispose))
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

        return Err(Error::syntax_error(
            expr.span(),
            "computed property names not allowed",
        ));
    }
}
