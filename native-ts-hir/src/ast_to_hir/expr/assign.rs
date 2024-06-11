use native_ts_parser::swc_core::common::{Span, Spanned};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::Transformer;
use crate::ast_to_hir::ValueBinding;
use crate::error::Error;
use crate::hir::{Expr, PropNameOrExpr, PropertyDesc, Type};
use crate::PropName;

use super::Result;

impl Transformer {
    /// translate assignment expression
    pub fn translate_assign_expr(&mut self, a: &swc::AssignExpr) -> Result<(Expr, Type)> {
        let (value, value_ty) = self.translate_expr(&a.right, None)?;

        return self.translate_assign_target(a.span, &a.left, a.op, value, value_ty);
    }

    pub fn translate_assign_target(
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
                        // return error
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
                        // return error
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

    /// translate variable assignment
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

        // cast value type if not the same
        if value_ty != var_ty {
            self.cast(&mut value, &value_ty, &var_ty);
        };

        // check if operand type is valid for assignment
        self.check_assign_op_type(span, op, &var_ty)?;

        // return variable assignment expression
        return Ok((
            Expr::VarAssign {
                op: op.into(),
                variable: varid,
                value: Box::new(value),
            },
            var_ty,
        ));
    }

    /// translate member assignment
    pub fn translate_member_assign(
        &mut self,
        span: Span,
        member: &swc::MemberExpr,
        op: swc::AssignOp,
        mut value: Expr,
        value_ty: Type,
    ) -> Result<(Expr, Type)> {
        // translate the memebr expression
        let (member_expr, member_ty) = self.translate_member_expr(member)?;

        // check that value type satisfies member type
        self.type_check(span, &value_ty, &member_ty)?;

        // cast
        if value_ty != member_ty {
            self.cast(&mut value, &value_ty, &member_ty);
        }

        if let Expr::Member {
            object,
            key,
            optional,
        } = member_expr
        {
            // opt chain is not allowed
            if optional {
                return Err(Error::syntax_error(
                    member.span(),
                    "invalid left-hand side assignment",
                ));
            }

            // check assignment is valid
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

    /// translate pattern assignment
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
                            op: crate::hir::AssignOp::Assign,
                            object: object,
                            key: key,
                            value: Box::new(value),
                        },
                        target_ty,
                    )),
                    Expr::VarLoad { span: _, variable } => Ok((
                        Expr::VarAssign {
                            op: crate::hir::AssignOp::Assign,
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

        return Ok((
            Expr::Tuple {
                span: pat.span,
                values: exprs,
            },
            Type::Tuple(tys.into()),
        ));
    }
}
