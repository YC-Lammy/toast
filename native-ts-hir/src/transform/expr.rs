use num_traits::ToPrimitive;

use native_ts_parser::swc_core::common::{Span, Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast::PropertyDesc;
use crate::common::{FunctionId, OR};
use crate::error::Error;
use crate::{
    ast::{Callee, Expr, FuncType, PropNameOrExpr, Type},
    PropName,
};

use super::Transformer;
use super::{ClassBinding, ValueBinding};

type Result<T> = std::result::Result<T, Error>;

pub enum MaybeTranslatedExpr<'a> {
    NotTranslated(&'a swc::Expr),
    Translated(Expr, Type),
}

impl<'a> MaybeTranslatedExpr<'a> {
    pub fn translate(
        self,
        transformer: &mut Transformer,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match self {
            Self::NotTranslated(e) => return transformer.translate_expr(e, expected_ty),
            Self::Translated(e, ty) => Ok((e, ty)),
        }
    }
}

impl Transformer {
    /// entry for translateing expressions
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
            // await expression
            swc::Expr::Await(a) => {
                // translate the promise
                let (e, mut ty) = self.translate_expr(&a.arg, None)?;

                // get the result type of promise
                if let Type::Promise(p) = ty {
                    ty = *p;
                };

                // get the result type of promise in union
                if let Type::Union(u) = ty {
                    // create new union
                    let mut v = Vec::with_capacity(u.len());

                    for t in u.iter() {
                        // type is promise
                        if let Type::Promise(p) = t {
                            // push promise result type
                            v.push(p.as_ref().clone());
                        } else {
                            // push the type
                            v.push(t.clone())
                        }
                    }
                    // write union
                    ty = Type::Union(v.into())
                }

                (Expr::Await(Box::new(e)), ty)
            }
            // binary expression
            swc::Expr::Bin(b) => self.translate_bin(b)?,
            // function call expression
            swc::Expr::Call(c) => self.translate_call(c)?,
            // class exression
            swc::Expr::Class(c) => {
                // class can only be declared as a type
                return Err(Error::syntax_error(
                    c.span(),
                    "class expression not allowed",
                ));
            }
            // conditional expression
            swc::Expr::Cond(cond) => self.translate_cond(cond)?,
            // function expression
            swc::Expr::Fn(f) => {
                let id = self.hoist_function(None, &f.function)?;
                self.translate_function(id, None, &f.function)?;

                let ty = self
                    .context
                    .functions
                    .get(&id)
                    .expect("invalid function")
                    .ty();

                (Expr::Closure(id), Type::Function(ty.into()))
            }
            swc::Expr::Ident(id) => self.translate_var_load(id)?,
            swc::Expr::This(t) => (Expr::This(t.span), self.this_ty.clone()),
            swc::Expr::Object(o) => self.translate_object_lit(o, expected_ty)?,
            swc::Expr::Unary(u) => self.translate_unary_expr(u)?,
            swc::Expr::Update(u) => self.translate_update_expr(u)?,
            swc::Expr::Member(m) => self.translate_member(m)?,
            swc::Expr::SuperProp(s) => self.translate_super_prop_expr(s)?,
            swc::Expr::New(n) => self.translate_new_expr(n)?,
            swc::Expr::Seq(s) => self.translate_seq_expr(s, expected_ty)?,
            swc::Expr::Lit(l) => self.translate_lit(l, expected_ty)?,
            // todo: string template
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
                    ty = Type::Union(v.into());
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
                self.cast(&mut e, &ty, expected)
            }

            return Ok((e, expected.clone()));
        }

        return Ok((e, ty));
    }

    /// array construction expression
    fn translate_array_expr(
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

                    self.type_check(span, &ty, &expected_elem_ty)?;

                    if !expected_elem_ty.as_ref().eq(&ty) {
                        self.cast(&mut expr, &ty, expected_elem_ty);
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

                    match t {
                        Type::Int | Type::LiteralInt(_) => {
                            t = Type::Number;
                            // cast expression to number
                            self.cast(&mut expr, &Type::Int, &Type::Number);
                        }
                        Type::LiteralNumber(_) => {
                            t = Type::Number;
                        }
                        Type::LiteralBigint(_) => {
                            t = Type::Bigint;
                        }
                        Type::LiteralBool(_) => {
                            t = Type::Bool;
                        }
                        Type::LiteralString(_) => t = Type::String,
                        _ => {}
                    };

                    if let Some(chained) = ty {
                        ty = Some(chained.union(t));
                    } else {
                        ty = Some(t);
                    };
                    values.push(expr);
                }

                let array_ty = ty
                    .map(|t| Type::Array(t.into()))
                    .or_else(|| Some(Type::Array(Type::Any.into())))
                    .unwrap();

                return Ok((Expr::Array { values: values }, array_ty));
            }
        }
    }

    fn translate_assign(&mut self, a: &swc::AssignExpr) -> Result<(Expr, Type)> {
        let (value, value_ty) = self.translate_expr(&a.right, None)?;

        return self.translate_assign_target(a.span, &a.left, a.op, value, value_ty);
    }

    fn translate_assign_target(
        &mut self,
        span: Span,
        target: &swc::AssignTarget,
        op: swc::AssignOp,
        value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        match target {
            // trnalate simple assignments
            swc::AssignTarget::Simple(simple) => match simple {
                swc::SimpleAssignTarget::Ident(id) => {
                    // variable assignment
                    self.translate_var_assign(span, &id.id, op, value, value_ty)
                }
                swc::SimpleAssignTarget::Member(m) => {
                    // member assignment
                    self.translate_member_assign(span, m, op, value, value_ty)
                }
                swc::SimpleAssignTarget::Invalid(i) => {
                    // invalid target
                    Err(Error::syntax_error(i.span, "invalid assignment target"))
                }
                // todo: simple assignments
                _ => todo!("assignment"),
            },
            // translate pattern assignments
            swc::AssignTarget::Pat(p) => match p {
                swc::AssignTargetPat::Object(pat) => {
                    // only assignment is allowed
                    if op != swc::AssignOp::Assign {
                        return Err(Error::syntax_error(
                            span,
                            format!("operator '{}' is not allowed on type 'object'", op.as_str()),
                        ));
                    }
                    // translate object pattern assignment
                    self.translate_object_pat_assign(pat, value, value_ty)
                }
                swc::AssignTargetPat::Array(pat) => {
                    // only assignment is allowed
                    if op != swc::AssignOp::Assign {
                        return Err(Error::syntax_error(
                            span,
                            format!("operator '{}' is not allowed on type 'object'", op.as_str()),
                        ));
                    }
                    // translate array pattern assignment
                    self.translate_array_pat_assign(pat, value, value_ty)
                }
                swc::AssignTargetPat::Invalid(i) => {
                    // invalid pattern
                    Err(Error::syntax_error(i.span, "invalid pattern"))
                }
            },
        }
    }

    /// check the operand type of an assignment operation
    fn check_assign_op_type(&mut self, span: Span, op: swc::AssignOp, ty: &Type) -> Result<()> {
        match op {
            swc::AssignOp::Assign => {}
            swc::AssignOp::AddAssign => match ty {
                Type::Bigint
                | Type::LiteralBigint(_)
                | Type::Number
                | Type::LiteralNumber(_)
                | Type::Int
                | Type::LiteralInt(_)
                | Type::String
                | Type::LiteralString(_) => {}
                _ => {
                    return Err(Error::syntax_error(
                        span,
                        "operator '+=' only accepts number, bigint and string",
                    ))
                }
            },
            swc::AssignOp::SubAssign
            | swc::AssignOp::MulAssign
            | swc::AssignOp::DivAssign
            | swc::AssignOp::ExpAssign
            | swc::AssignOp::ModAssign
            | swc::AssignOp::LShiftAssign
            | swc::AssignOp::RShiftAssign
            | swc::AssignOp::ZeroFillRShiftAssign
            | swc::AssignOp::BitAndAssign
            | swc::AssignOp::BitOrAssign
            | swc::AssignOp::BitXorAssign => match ty {
                Type::Bigint
                | Type::LiteralBigint(_)
                | Type::Number
                | Type::LiteralNumber(_)
                | Type::Int
                | Type::LiteralInt(_) => {}
                _ => {
                    return Err(Error::syntax_error(
                        span,
                        format!("operator '{}' only accepts number and bigint", op.as_str()),
                    ))
                }
            },
            swc::AssignOp::NullishAssign => {}
            swc::AssignOp::OrAssign => {}
            swc::AssignOp::AndAssign => {
                if ty != &Type::Bool {
                    return Err(Error::syntax_error(
                        span,
                        "operator '&&=' only accepts boolean",
                    ));
                }
            }
        };

        return Ok(());
    }

    pub fn translate_var_assign(
        &mut self,
        span: Span,
        var: &swc::Ident,
        op: swc::AssignOp,
        mut value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        // get the variable id and variable type
        let (varid, var_ty) = if let Some(binding) = self.context.find_value_binding(&var.sym) {
            match binding {
                ValueBinding::Var {
                    writable,
                    redeclarable: _,
                    id,
                    ty,
                } => {
                    // constants are not writable
                    if !*writable {
                        return Err(Error::syntax_error(
                            var.span,
                            format!("variable '{}' is immutable", &var.sym),
                        ));
                    }
                    // return id and type
                    (*id, ty.clone())
                }
                // using declare cannot be mutated
                ValueBinding::Using { .. } => {
                    return Err(Error::syntax_error(
                        var.span,
                        format!("variable '{}' is immutable", &var.sym),
                    ))
                }
                // all other bindings are not considered variable
                _ => {
                    return Err(Error::syntax_error(
                        var.span,
                        format!("identifier '{}' is not a variable", &var.sym),
                    ))
                }
            }
        } else {
            return Err(Error::syntax_error(
                var.span,
                format!("undeclared identifier '{}'", var.sym),
            ));
        };

        // type check
        self.type_check(var.span, &value_ty, &var_ty)?;

        if value_ty != var_ty {
            self.cast(&mut value, &value_ty, &var_ty);
        };

        // check if operand type is valid for assignment
        self.check_assign_op_type(span, op, &var_ty)?;

        return Ok((
            Expr::VarAssign {
                op: op.into(),
                variable: varid,
                value: Box::new(value),
            },
            var_ty,
        ));
    }

    pub fn translate_member_assign(
        &mut self,
        span: Span,
        member: &swc::MemberExpr,
        op: swc::AssignOp,
        mut value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        let (member_expr, member_ty) = self.translate_member(member)?;

        self.type_check(span, &value_ty, &member_ty)?;

        if value_ty != member_ty {
            self.cast(&mut value, &value_ty, &member_ty);
        }

        if let Expr::Member {
            object,
            key,
            optional,
        } = member_expr
        {
            if optional {
                return Err(Error::syntax_error(
                    member.span(),
                    "invalid left-hand side assignment",
                ));
            }

            self.check_assign_op_type(span, op, &member_ty)?;

            return Ok((
                Expr::MemberAssign {
                    op: op.into(),
                    object: object,
                    key: key,
                    value: Box::new(value),
                },
                member_ty,
            ));
        } else {
            unreachable!()
        }
    }

    pub fn translate_object_pat_assign(
        &mut self,
        pat: &swc::ObjectPat,
        value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        // top level pattern cannot be optional
        if pat.optional {
            return Err(Error::syntax_error(
                pat.span,
                "object pattern cannot be optional",
            ));
        }

        // type annotation not allowed
        if let Some(ann) = &pat.type_ann {
            return Err(Error::syntax_error(ann.span, "type annotation not allowed"));
        }

        // result object expression
        let mut obj_expr = Vec::new();
        // result object type
        let mut obj_ty = Vec::new();
        // counter
        let mut i = 0;

        // translate property assignment
        for prop in &pat.props {
            // is first property
            let is_first = i == 0;
            // is last property
            let is_last = i == pat.props.len() - 1;
            // increment counter
            i += 1;

            match prop {
                swc::ObjectPatProp::Assign(a) => {
                    // construct prop name
                    let prop = PropName::Ident(a.key.id.sym.to_string());
                    // default value
                    if let Some(_) = &a.value {
                        // todo: assignment pattern
                        return Err(Error::syntax_error(
                            a.span,
                            "default assignment is not allowed",
                        ));
                    }
                    // type annotation not allowed
                    if let Some(ann) = &a.key.type_ann {
                        return Err(Error::syntax_error(ann.span, "type annotation not allowed"));
                    }

                    // value type of property
                    let value_ty =
                        if let Some(value_ty) = self.type_has_property(&value_ty, &prop, false) {
                            value_ty
                        } else {
                            return Err(Error::syntax_error(
                                a.span,
                                format!("value has no property '{}'", a.key.id.sym),
                            ));
                        };

                    let v = if is_first && is_last {
                        // todo: avoid clone
                        value.clone()
                    } else if is_first {
                        // todo: avoid clone
                        Expr::Push(Box::new(value.clone()))
                    } else if is_last {
                        Expr::Pop
                    } else {
                        Expr::ReadStack
                    };

                    let v = Expr::Member {
                        object: Box::new(v),
                        key: PropNameOrExpr::PropName(prop.clone()),
                        optional: false,
                    };

                    let (assign_expr, assign_ty) = self.translate_var_assign(
                        a.span,
                        &a.key.id,
                        swc::AssignOp::Assign,
                        v,
                        value_ty,
                    )?;

                    obj_expr.push((prop.clone(), assign_expr));
                    obj_ty.push(PropertyDesc {
                        name: prop,
                        ty: assign_ty,
                        readonly: false,
                    });
                }
                swc::ObjectPatProp::KeyValue(k) => {
                    let key = self.translate_prop_name(&k.key)?;
                    let key = match key {
                        PropNameOrExpr::PropName(p) => p,
                        PropNameOrExpr::Expr(_, _) => unimplemented!(),
                    };

                    let v = if is_first && is_last {
                        // todo: avoid clone
                        value.clone()
                    } else if is_first {
                        // todo: avoid clone
                        Expr::Push(Box::new(value.clone()))
                    } else if is_last {
                        Expr::Pop
                    } else {
                        Expr::ReadStack
                    };

                    let v = Expr::Member {
                        object: Box::new(v),
                        key: PropNameOrExpr::PropName(key.clone()),
                        optional: false,
                    };

                    if let Some(value_ty) = self.type_has_property(&value_ty, &key, false) {
                        // translate pattern assignment
                        let (expr, assign_ty) = self.translate_pat_assign(
                            &k.value,
                            swc::AssignOp::Assign,
                            v,
                            value_ty,
                        )?;

                        obj_expr.push((key.clone(), expr));
                        obj_ty.push(PropertyDesc {
                            name: key,
                            ty: assign_ty,
                            readonly: false,
                        });
                    } else {
                        return Err(Error::syntax_error(
                            k.key.span(),
                            format!("type '' has no property '{}'", key),
                        ));
                    }
                }
                swc::ObjectPatProp::Rest(r) => {
                    // todo: rest assignment
                    return Err(Error::syntax_error(
                        r.dot3_token,
                        "rest assignment not supported",
                    ));
                }
            }
        }

        obj_ty.sort();

        return Ok((
            Expr::Object { props: obj_expr },
            Type::LiteralObject(obj_ty.into()),
        ));
    }

    pub fn translate_array_pat_assign(
        &mut self,
        pat: &swc::ArrayPat,
        value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        if pat.optional {
            if value_ty == Type::Undefined {
                return Ok((Expr::Undefined, Type::Undefined));
            }
        }
        if let Some(ann) = &pat.type_ann {
            return Err(Error::syntax_error(ann.span, "type annotation not allowed"));
        }

        let mut exprs = Vec::new();
        let mut tys = Vec::new();

        let mut obj = Expr::Push(Box::new(value));

        for i in 0..pat.elems.len() {
            let is_first = i == 0;
            let is_last = i == pat.elems.len() - 1;

            // translate the required assignment expression for the element
            let (expr, ty) = if let Some(elem) = &pat.elems[i] {
                // check if the target value has index property
                if let Some(value_ty) =
                    self.type_has_property(&value_ty, &PropName::Int(i as _), false)
                {
                    // get the object from stack
                    let obj = if is_first && is_last {
                        // get the expression directly
                        if let Expr::Push(v) = core::mem::replace(&mut obj, Expr::Undefined) {
                            *v
                        } else {
                            unreachable!()
                        }
                    } else if is_first {
                        let o = obj;
                        obj = Expr::ReadStack;
                        o
                    } else if is_last {
                        // pop from stack
                        Expr::Pop
                    } else {
                        // read from stack
                        Expr::ReadStack
                    };

                    // translate pattern assignment of element
                    let (expr, value_ty) = self.translate_pat_assign(
                        elem,
                        swc::AssignOp::Assign,
                        Expr::Member {
                            // the object
                            object: Box::new(obj),
                            // the index
                            key: PropNameOrExpr::PropName(PropName::Int(i as _)),
                            // not optional
                            optional: false,
                        },
                        value_ty,
                    )?;

                    // return the assignment expression and type
                    (expr, value_ty)
                } else {
                    // no index property is found, check if assignment is optional
                    let is_optional = match elem {
                        swc::Pat::Array(a) => a.optional,
                        // todo: assign pattern
                        swc::Pat::Assign(a) => {
                            return Err(Error::syntax_error(
                                a.span,
                                "assignment pattern not supported",
                            ))
                        }
                        // expr pattern is only valid in for in loops
                        swc::Pat::Expr(_) => unreachable!(),
                        swc::Pat::Ident(id) => id.optional,
                        swc::Pat::Invalid(i) => {
                            return Err(Error::syntax_error(
                                i.span,
                                "invalid left-hand side assignment",
                            ))
                        }
                        swc::Pat::Object(o) => o.optional,
                        // todo: rest assignment
                        swc::Pat::Rest(r) => {
                            return Err(Error::syntax_error(
                                r.dot3_token,
                                "rest assignment not supported",
                            ))
                        }
                    };

                    // return undefined if optional
                    if is_optional {
                        (Expr::Undefined, Type::Undefined)
                    } else {
                        // has no property and not optional, return error
                        return Err(Error::syntax_error(
                            elem.span(),
                            format!("type '' has no property '{}'", i),
                        ));
                    }
                }
            } else {
                // there is no assignment in this index, return undefined
                (Expr::Undefined, Type::Undefined)
            };

            // push to array type construction
            tys.push(ty);
            // push expression to array
            exprs.push(expr);
        }

        return Ok((Expr::Tuple { values: exprs }, Type::Tuple(tys.into())));
    }

    fn translate_pat_assign(
        &mut self,
        pat: &swc::Pat,
        op: swc::AssignOp,
        value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        match pat {
            // destructive array assignment
            swc::Pat::Array(a) => self.translate_array_pat_assign(a, value, value_ty),
            // destructive object assignment
            swc::Pat::Object(o) => self.translate_object_pat_assign(o, value, value_ty),
            // todo: assignment pattern
            swc::Pat::Assign(a) => {
                return Err(Error::syntax_error(
                    a.span,
                    "assignment pattern not supported",
                ))
            }
            // expr pattern is only valid in for-in loops
            swc::Pat::Expr(e) => {
                let (assign_target, target_ty) = self.translate_expr(&e, None)?;

                self.type_check(e.span(), &value_ty, &target_ty)?;

                let mut value = value;
                self.cast(&mut value, &value_ty, &target_ty);

                match assign_target {
                    Expr::Member {
                        object,
                        key,
                        // invalid assignment target if optional
                        optional: false,
                    } => Ok((
                        Expr::MemberAssign {
                            op: crate::ast::AssignOp::Assign,
                            object: object,
                            key: key,
                            value: Box::new(value),
                        },
                        target_ty,
                    )),
                    Expr::VarLoad { span: _, variable } => Ok((
                        Expr::VarAssign {
                            op: crate::ast::AssignOp::Assign,
                            variable: variable,
                            value: Box::new(value),
                        },
                        target_ty,
                    )),
                    _ => {
                        return Err(Error::syntax_error(
                            e.span(),
                            "invalid left-hand side assignment",
                        ))
                    }
                }
            }
            // simple variable assignment
            swc::Pat::Ident(id) => {
                self.translate_var_assign(pat.span(), &id.id, op, value, value_ty)
            }
            swc::Pat::Invalid(i) => {
                return Err(Error::syntax_error(
                    i.span,
                    "invalid left-hand side assignment",
                ))
            }
            // todo: rest assignment
            swc::Pat::Rest(r) => {
                return Err(Error::syntax_error(
                    r.dot3_token,
                    "rest assignment is not supported",
                ))
            }
        }
    }

    fn translate_bin(&mut self, b: &swc::BinExpr) -> Result<(Expr, Type)> {
        if b.op == swc::BinaryOp::In {
            let prop = self.translate_computed_prop_name(&b.left)?;
            let (right, right_ty) = self.translate_expr(&b.right, None)?;

            // translate the left hand side
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
                    // todo: computed prop name
                    return Err(Error::syntax_error(
                        b.span,
                        "computed property name 'in' operation not supported",
                    ));
                }
            }
        }
        let (mut left, mut left_ty) = self.translate_expr(&b.left, None)?;
        let (mut right, mut right_ty) = self.translate_expr(&b.right, None)?;

        // treat literal int as int
        if let Type::LiteralInt(_) = left_ty {
            left_ty = Type::Int;
        }
        if let Type::LiteralInt(_) = right_ty {
            right_ty = Type::Int;
        }
        // treat literal number as number
        if let Type::LiteralNumber(_) = left_ty {
            left_ty = Type::Number;
        }
        if let Type::LiteralNumber(_) = right_ty {
            right_ty = Type::Number;
        }
        // treat literal bigint as bigint
        if let Type::LiteralBigint(_) = left_ty {
            left_ty = Type::Bigint;
        }
        if let Type::LiteralBigint(_) = right_ty {
            right_ty = Type::Bigint;
        }
        // treat literal bool as bool
        if let Type::LiteralBool(_) = left_ty {
            left_ty = Type::Bool;
        }
        if let Type::LiteralBool(_) = right_ty {
            right_ty = Type::Bool;
        }
        // treat literal string as string
        if let Type::LiteralString(_) = left_ty {
            left_ty = Type::String;
        }
        if let Type::LiteralString(_) = right_ty {
            right_ty = Type::String;
        }

        // the result type
        let ty;

        // check op and the result type
        match b.op {
            swc::BinaryOp::Add
            | swc::BinaryOp::Sub
            | swc::BinaryOp::Mul
            | swc::BinaryOp::Mod
            | swc::BinaryOp::Exp => {
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    // both are int
                    ty = Type::Int;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    // cast left hand side as number
                    left_ty = Type::Number;
                    // cast expression
                    self.cast(&mut left, &Type::Int, &Type::Number);
                    // result is number
                    ty = Type::Number;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    // cast right hand side as number
                    right_ty = Type::Number;
                    // cast expression
                    self.cast(&mut right, &Type::Int, &Type::Number);
                    // result is number
                    ty = Type::Number;
                // both are bigint
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                // both are string
                } else if b.op == swc::BinaryOp::Add
                    && left_ty == Type::String
                    && right_ty == Type::String
                {
                    ty = Type::String
                } else {
                    // unsupported types
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // right should be equal to left
                debug_assert!(right_ty == left_ty);
                debug_assert!(
                    left_ty == Type::Int
                        || left_ty == Type::Number
                        || left_ty == Type::Bigint
                        || left_ty == Type::String
                );
            }
            swc::BinaryOp::Div => {
                if left_ty == Type::Int {
                    left_ty = Type::Number;
                    self.cast(&mut left, &Type::Int, &Type::Number);
                }
                if right_ty == Type::Int {
                    right_ty = Type::Number;
                    self.cast(&mut right, &Type::Int, &Type::Number);
                }
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Number;
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bigint;
                } else {
                    // unsupported types
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // right should be equal to left
                debug_assert!(right_ty == left_ty);
                debug_assert!(
                    left_ty == Type::Int || left_ty == Type::Number || left_ty == Type::Bigint
                );
            }
            swc::BinaryOp::BitAnd
            | swc::BinaryOp::BitOr
            | swc::BinaryOp::BitXor
            | swc::BinaryOp::LShift
            | swc::BinaryOp::RShift
            | swc::BinaryOp::ZeroFillRShift => {
                if left_ty == Type::Number {
                    // cast to int
                    left_ty = Type::Int;
                    self.cast(&mut left, &Type::Number, &Type::Int);
                }
                if right_ty == Type::Number {
                    // cast to int
                    right_ty = Type::Int;
                    self.cast(&mut right, &Type::Number, &Type::Int);
                }
                // both sides must be int or bigint
                if !(left_ty == Type::Int && right_ty == Type::Int)
                    && !(left_ty == Type::Bigint && right_ty == Type::Bigint)
                {
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                // left should be equal to right
                debug_assert!(left_ty == right_ty);
                debug_assert!(left_ty == Type::Int || left_ty == Type::Bigint);

                // either bigint or int
                ty = left_ty;
            }
            swc::BinaryOp::EqEq
            | swc::BinaryOp::EqEqEq
            | swc::BinaryOp::NotEq
            | swc::BinaryOp::NotEqEq => {
                // result must be boolean
                ty = Type::Bool;
            }
            swc::BinaryOp::Gt | swc::BinaryOp::GtEq | swc::BinaryOp::Lt | swc::BinaryOp::LtEq => {
                // both are number
                if left_ty == Type::Number && right_ty == Type::Number {
                    ty = Type::Bool;
                // both are int
                } else if left_ty == Type::Int && right_ty == Type::Int {
                    ty = Type::Bool;
                } else if left_ty == Type::Int && right_ty == Type::Number {
                    // cast left to number
                    left_ty = Type::Number;
                    self.cast(&mut left, &Type::Int, &Type::Number);
                    ty = Type::Bool;
                } else if left_ty == Type::Number && right_ty == Type::Int {
                    // cast right to number
                    right_ty = Type::Number;
                    self.cast(&mut right, &Type::Number, &Type::Int);
                    ty = Type::Bool;
                // both are bigint
                } else if left_ty == Type::Bigint && right_ty == Type::Bigint {
                    ty = Type::Bool;
                } else {
                    // not bigint, number or int
                    return Err(Error::syntax_error(b.span, format!("The operand of an arithmetic operation must be of type 'number' or 'bigint'")));
                }

                debug_assert!(right_ty == left_ty);
            }
            swc::BinaryOp::NullishCoalescing | swc::BinaryOp::LogicalOr => {
                ty = left_ty.union(right_ty);
            }
            swc::BinaryOp::LogicalAnd => {
                if left_ty != Type::Bool {
                    self.cast(&mut left, &left_ty, &Type::Bool);
                    left_ty = Type::Bool;
                }
                if right_ty != Type::Bool {
                    self.cast(&mut right, &right_ty, &Type::Bool);
                    right_ty = Type::Bool;
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
    fn translate_call(&mut self, call: &swc::CallExpr) -> Result<(Expr, Type)> {
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

                (Callee::Super(sup), func_ty.into())
            }
            swc::Callee::Import(i) => {
                return Err(Error::syntax_error(i.span, "dynamic import not allowed"))
            }
            swc::Callee::Expr(e) => {
                // translate the expression
                let (expr, ty) = self.translate_expr(e, None)?;

                // check it is a function
                let func_ty = if let Type::Function(func) = ty {
                    func
                } else {
                    return Err(Error::syntax_error(call.span, "callee is not a function"));
                };

                // convert to callee
                match expr {
                    Expr::Member {
                        object,
                        key,
                        optional,
                    } => {
                        if optional {
                            return Err(Error::syntax_error(
                                call.callee.span(),
                                format!("Type '' is not callable",),
                            ));
                        }
                        // check if object matches func_ty.this type
                        // TODO
                        (
                            Callee::Member {
                                object: *object,
                                prop: key,
                                optional: false,
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
            let (mut a, arg_ty) = self.translate_expr(&arg.expr, expected_arguments.get(i))?;

            // check argument fulfills type
            self.type_check(arg.span(), &arg_ty, &expected_arguments[i])?;

            if &arg_ty != &expected_arguments[i] {
                // convert value to type
                self.cast(&mut a, &arg_ty, &expected_arguments[i]);
            }

            // push expression to arguments
            args.push(a);
        }

        return Ok((
            Expr::Call {
                callee: Box::new(callee),
                args: args,
                optional: false,
            },
            callee_ty.return_ty.clone(),
        ));
    }

    fn translate_cond(&mut self, cond: &swc::CondExpr) -> Result<(Expr, Type)> {
        let (mut test, test_ty) = self.translate_expr(&cond.test, None)?;
        let (cons, cons_ty) = self.translate_expr(&cond.cons, None)?;
        let (alt, alt_ty) = self.translate_expr(&cond.alt, None)?;

        if test_ty != Type::Bool {
            // cast it to bool
            self.cast(&mut test, &test_ty, &Type::Bool);
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

    fn translate_var_load(&mut self, ident: &swc::Ident) -> Result<(Expr, Type)> {
        // if it is namespace, it is certainly not a value
        match self.context.find_namespace_or_value_binding(&ident.sym){
            Some(OR::A(namespace)) => {
                return Ok((Expr::NamespaceObject(namespace), Type::NamespaceObject(namespace)))
            }
            Some(OR::B(ValueBinding::Var {
                id,
                ty, ..
            })) | Some(OR::B(ValueBinding::Using { id, ty, .. }))
            =>{
                return Ok((
                    Expr::VarLoad {
                        span: ident.span,
                        variable: *id,
                    },
                    ty.clone(),
                ));
            }
            Some(OR::B(ValueBinding::Function(f))) => {
                // copy the id to avoid borrowing self
                let id = *f;
                // get the functio type
                let ty = self
                    .context
                    .functions
                    .get(&id)
                    .expect("invalid function id")
                    .ty();
                // return expression
                return Ok((Expr::Function(id), Type::Function(ty.into())));
            }
            Some(OR::B(ValueBinding::GenericFunction(f))) => {
                return Err(Error::syntax_error(ident.span, "missing type arguments"))
            }
            None => {if self.context.has_binding(&ident.sym) {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("identifier '{}' is not a variable", ident.sym),
                    ));
                } else {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("undefined identifier '{}'", ident.sym),
                    ));
                }}
        }
    }

    fn translate_member(&mut self, member: &swc::MemberExpr) -> Result<(Expr, Type)> {
        
        let prop = match &member.prop {
            swc::MemberProp::Computed(c) => self.translate_computed_prop_name(&c.expr)?,
            swc::MemberProp::Ident(id) => {
                PropNameOrExpr::PropName(PropName::Ident(id.sym.to_string()))
            }
            swc::MemberProp::PrivateName(id) => {
                PropNameOrExpr::PropName(PropName::Private(id.id.sym.to_string()))
            }
        };

        let (obj, obj_ty) = self.translate_expr(&member.obj, None)?;

        if let PropNameOrExpr::PropName(PropName::Private(_)) = &prop{
            if self.this_ty != obj_ty{
                return Err(Error::syntax_error(
                        member.prop.span(),
                        "cannot access privite properties outside of method",
                    ));
            }
        }
        

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
                            self.cast(&mut e, &ty, k.as_ref());
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
                    Type::Array(elem) => match ty {
                        Type::Int | Type::LiteralInt(_) | Type::Number | Type::LiteralNumber(_) => {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                elem.as_ref().clone(),
                            ));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                member.span,
                                "array can only be indexed by number",
                            ))
                        }
                    },
                    Type::Tuple(elems) => match ty {
                        Type::Int | Type::LiteralInt(_) | Type::Number | Type::LiteralNumber(_) => {
                            return Ok((
                                Expr::Member {
                                    object: Box::new(obj),
                                    key: PropNameOrExpr::Expr(e, ty),
                                    optional: false,
                                },
                                Type::Union(elems.clone()),
                            ));
                        }
                        _ => {
                            return Err(Error::syntax_error(
                                member.span,
                                "tuple can only be indexed by number",
                            ));
                        }
                    },
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

    fn translate_unary_expr(&mut self, u: &swc::UnaryExpr) -> Result<(Expr, Type)> {
        let (mut expr, ty) = self.translate_expr(&u.arg, None)?;
        match u.op {
            swc::UnaryOp::Bang => {
                if ty != Type::Bool {
                    // cast type to bool
                    self.cast(&mut expr, &ty, &Type::Bool)
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
                match ty {
                    Type::Number
                    | Type::LiteralNumber(_)
                    | Type::Int
                    | Type::LiteralInt(_)
                    | Type::Bigint
                    | Type::LiteralBigint(_) => {
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
                    }
                    _ => {
                        return Err(Error::syntax_error(
                            u.span,
                            "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                        ))
                    }
                }
            }
            swc::UnaryOp::Tilde => {
                match ty {
                    Type::Number => {}
                    Type::Int => {}
                    Type::LiteralNumber(_) => {}
                    Type::LiteralInt(_) => {}
                    Type::Bigint => {}
                    Type::LiteralBigint(_) => {}
                    Type::Bool => {}
                    Type::LiteralBool(_) => {}
                    _ => {
                        return Err(Error::syntax_error(
                            u.span,
                            "right-hand side must be one of 'number', 'bigint' or 'boolean'",
                        ));
                    }
                }

                self.cast(&mut expr, &ty, &Type::Int);

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
                // runtime reflect
                return Ok((
                    Expr::Unary {
                        op: crate::ast::UnaryOp::Typeof,
                        value: Box::new(expr),
                    },
                    Type::String,
                ));
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

    fn translate_update_expr(&mut self, u: &swc::UpdateExpr) -> Result<(Expr, Type)> {
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

        match ty {
            Type::Int
            | Type::LiteralInt(_)
            | Type::Number
            | Type::LiteralNumber(_)
            | Type::Bigint
            | Type::LiteralBigint(_) => {}
            _ => {
                return Err(Error::syntax_error(
                    u.span,
                    "operand must have type 'number' or 'bigint'",
                ))
            }
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

    fn translate_super_prop_expr(&mut self, s: &swc::SuperPropExpr) -> Result<(Expr, Type)> {
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
                    return Ok((Expr::Function(*fid), Type::Function(ty.clone().into())));
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
                        Expr::Cast(Box::new(Expr::This(s.obj.span)), Type::Object(super_class)),
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

    fn translate_new_expr(&mut self, n: &swc::NewExpr) -> Result<(Expr, Type)> {
        let ident = match n.callee.as_ident() {
            Some(ident) => ident,
            None => return Err(Error::syntax_error(n.callee.span(), "expected identifier")),
        };

        let mut arguments = Vec::new();

        match self.context.find_class_binding(&ident.sym) {
            Some(ClassBinding::Class(class_id)) => {
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
            Some(ClassBinding::Generic(_class_id)) => {
                todo!("generic class")
            }
            None => {
                if self.context.has_binding(&ident.sym) {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("'{}' is not a constructor.", ident.sym),
                    ));
                } else {
                    return Err(Error::syntax_error(
                        ident.span,
                        format!("undefined identifier '{}'", ident.sym),
                    ));
                }
            }
        };
    }

    fn translate_seq_expr(
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

    fn translate_lit(
        &mut self,
        lit: &swc::Lit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        match lit {
            swc::Lit::BigInt(b) => {
                let i = b.value.to_i128().expect("i128 overflow");
                Ok((Expr::Bigint(i), Type::LiteralBigint(i)))
            }
            swc::Lit::Bool(b) => Ok((Expr::Bool(b.value), Type::LiteralBool(b.value))),
            swc::Lit::JSXText(_) => unimplemented!(),
            swc::Lit::Null(_) => Ok((Expr::Null, Type::Null)),
            swc::Lit::Num(n) => {
                if expected_ty == Some(&Type::Number) {
                    return Ok((Expr::Number(n.value), Type::Number));
                }
                if n.value.is_finite() && n.value as i32 as f64 == n.value {
                    return Ok((Expr::Int(n.value as i32), Type::LiteralInt(n.value as i32)));
                }

                return Ok((Expr::Number(n.value), Type::LiteralNumber(n.value.into())));
            }
            swc::Lit::Str(s) => Ok((
                Expr::String(s.value.to_string()),
                Type::LiteralString(s.value.as_str().into()),
            )),
            // todo: regex
            swc::Lit::Regex(_r) => Ok((Expr::Regex(), Type::Regex)),
        }
    }

    fn translate_object_lit(
        &mut self,
        obj: &swc::ObjectLit,
        expected_ty: Option<&Type>,
    ) -> Result<(Expr, Type)> {
        let mut spans = Vec::new();
        let mut tys = Vec::new();
        let mut values = Vec::new();

        for p in &obj.props {
            // translate the property name
            let propname = match p {
                swc::PropOrSpread::Spread(s) => {
                    return Err(Error::syntax_error(
                        s.dot3_token,
                        "spread expression not supported",
                    ))
                }
                swc::PropOrSpread::Prop(p) => match p.as_ref() {
                    // multiple clause to reduce footprint
                    swc::Prop::Assign(a) => PropName::Ident(a.key.sym.to_string()),
                    swc::Prop::KeyValue(swc::KeyValueProp { key, .. })
                    | swc::Prop::Method(swc::MethodProp { key, .. })
                    | swc::Prop::Setter(swc::SetterProp { key, .. })
                    | swc::Prop::Getter(swc::GetterProp { key, .. }) => {
                        match self.translate_prop_name(&key)? {
                            PropNameOrExpr::PropName(p) => p,
                            PropNameOrExpr::Expr(_, _) => {
                                return Err(Error::syntax_error(
                                    key.span(),
                                    "computed property name not allowed",
                                ))
                            }
                        }
                    }
                    swc::Prop::Shorthand(s) => PropName::Ident(s.sym.to_string()),
                },
            };

            let expected_prop_ty = if let Some(expected) = expected_ty {
                if let Some(ty) = self.type_has_property(expected, &propname, false) {
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };

            // translate property
            match p {
                swc::PropOrSpread::Spread(_) => unreachable!(),
                swc::PropOrSpread::Prop(p) => {
                    match p.as_ref() {
                        swc::Prop::KeyValue(v) => {
                            // translate value
                            let (mut value, mut ty) =
                                self.translate_expr(&v.value, expected_prop_ty.as_ref())?;

                            // generalise type
                            ty = self.generalise_type(&mut value, &ty).unwrap_or(ty);

                            spans.push(v.key.span());
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Shorthand(s) => {
                            if s.optional {
                                return Err(Error::syntax_error(
                                    s.span,
                                    "object literal shorthand cannot be optional",
                                ));
                            }

                            // translate variale load
                            let (mut value, ty) = self.translate_var_load(s)?;

                            // type check
                            let ty = if let Some(expected) = expected_prop_ty {
                                self.type_check(s.span, &ty, &expected)?;
                                self.cast(&mut value, &ty, &expected);
                                // use expected type
                                expected
                            } else {
                                ty
                            };

                            spans.push(s.span);
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Method(m) => {
                            // create new function id
                            let funcid = FunctionId::new();
                            // tranlate function
                            self.translate_function(funcid, None, &m.function)?;

                            // create closure
                            let mut value = Expr::Closure(funcid);
                            // function type
                            let ty = Type::Function(
                                self.context.functions.get(&funcid).unwrap().ty().into(),
                            );

                            // type check
                            let ty = if let Some(expected) = expected_prop_ty {
                                self.type_check(m.key.span(), &ty, &expected)?;
                                self.cast(&mut value, &ty, &expected);
                                // use expected type
                                expected
                            } else {
                                ty
                            };

                            spans.push(m.key.span());
                            // push property
                            tys.push(PropertyDesc {
                                name: propname.clone(),
                                ty: ty,
                                readonly: false,
                            });
                            // push expression
                            values.push((propname, value));
                        }
                        swc::Prop::Getter(g) => {
                            // todo: getter
                            return Err(Error::syntax_error(g.span, "getter not supported"));
                        }
                        swc::Prop::Setter(s) => {
                            // todo: setter
                            return Err(Error::syntax_error(s.span, "setter not supported"));
                        }
                        // this is invalid for object literal
                        swc::Prop::Assign(_) => unreachable!(),
                    }
                }
            }
        }

        // check for duplicated property names
        for (n, (p1, _)) in values.iter().enumerate() {
            for (i, (p2, _)) in values.iter().enumerate() {
                if n != i && p1 == p2 {
                    return Err(Error::syntax_error(spans[i], "duplicated property name"));
                }
            }
        }

        // add missing properties to the object
        if let Some(expected) = expected_ty {
            // loop through properties of expected type
            for p in self.get_properties(expected) {
                // missing property
                if values.iter().find(|(key, _)| key == &p.name).is_none() {
                    // undefined can be assigned to the property
                    if self.type_check(DUMMY_SP, &Type::Undefined, &p.ty).is_ok() {
                        // add property with value undefined
                        values.push((
                            p.name.clone(),
                            Expr::Cast(Box::new(Expr::Undefined), p.ty.clone()),
                        ));
                        // add property to type
                        tys.push(p.clone());
                    }
                }
            }
        }

        // sort the descriptors
        tys.sort();

        // object expression
        let mut obj_expr = Expr::Object { props: values };
        let obj_ty = Type::LiteralObject(tys.into());

        // type check
        let ty = if let Some(expected) = expected_ty {
            self.type_check(obj.span, &obj_ty, &expected)?;
            self.cast(&mut obj_expr, &obj_ty, &expected);

            expected.clone()
        } else {
            obj_ty
        };

        return Ok((obj_expr, ty));
    }

    fn translate_optchain(&mut self, n: &swc::OptChainExpr) -> Result<(Expr, Type)> {
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

                let ty_args = if let Some(type_args) = &c.type_args {
                    let mut args = Vec::new();

                    for t in &type_args.params {
                        args.push(self.translate_type(&t)?)
                    }

                    args
                } else {
                    Vec::new()
                };

                // function call
                if let Some(ident) = c.callee.as_ident() {
                    // find the function
                    match self.context.find_value_binding(&ident.sym) {
                        // a function
                        Some(ValueBinding::Function(f)) => {
                            // type arguments not allowed
                            if !ty_args.is_empty() {
                                return Err(Error::syntax_error(
                                    c.type_args.as_ref().unwrap().span,
                                    "Type '' has no signatures for which the type argument list is applicable.",
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
                        // a generic function
                        Some(ValueBinding::GenericFunction(_id)) => {
                            todo!("generic function")
                        }
                        _ => {}
                    };
                }

                // expression
                if callee.is_none() {
                    // translate the callee as expression
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
                            // will never call, return the expression
                            return Ok((expr, Type::Undefined));
                        }
                        Type::Null => {
                            // will never call, return the expression
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
                        callee = Some(Callee::Member {
                            object: *object,
                            prop: key,
                            optional: optional,
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

    pub(super) fn translate_prop_name(&mut self, name: &swc::PropName) -> Result<PropNameOrExpr> {
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

    pub(super) fn translate_computed_prop_name(
        &mut self,
        expr: &swc::Expr,
    ) -> Result<PropNameOrExpr> {
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

        return Ok(PropNameOrExpr::Expr(Box::new(expr), ty));
    }
}
