use native_ts_parser::swc_core::common::{Spanned, DUMMY_SP};
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::expr::MaybeTranslatedExpr;
use crate::error::Error;
use crate::hir::{AssignOp, VarKind, VariableDesc};
use crate::Symbol;
use crate::{
    common::VariableId,
    hir::{Callee, Expr, PropNameOrExpr, Stmt, Type},
    PropName,
};

use super::Result;
use super::Transformer;

impl Transformer {
    /// translate for...of loop
    pub fn translate_for_of_loop_stmt(
        &mut self,
        label: Option<&str>,
        stmt: &swc::ForOfStmt,
    ) -> Result<()> {
        let has_old_label = if let Some(label) = label {
            !self.break_labels.insert(label.into()) || !self.continue_labels.insert(label.into())
        } else {
            false
        };

        let iter_symbol = if stmt.is_await {
            Symbol::AsyncIterator
        } else {
            Symbol::Iterator
        };

        let right_span = stmt.right.span();

        // translate the iter
        let (target, target_ty) = self.translate_expr(&stmt.right, None)?;

        // variable to store iter
        let target_var_id = VariableId::new();
        // register variable
        self.context.func().variables.insert(
            target_var_id,
            VariableDesc {
                ty: target_ty.clone(),
                is_heap: false,
            },
        );
        // declare variable
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Let,
            id: target_var_id,
            ty: target_ty,
        });
        // assign value to variable
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                span: DUMMY_SP,
                op: AssignOp::Assign,
                variable: target_var_id,
                value: Box::new(target),
            })));

        // variable to store iterator
        let iterator_var_id = VariableId::new();
        // register variable
        self.context.func().variables.insert(
            iterator_var_id,
            VariableDesc {
                ty: Type::Any.into(),
                is_heap: false,
            },
        );
        // declare variable
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Let,
            id: iterator_var_id,
            ty: Type::Any.into(),
        });
        // assign value to variable
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                span: DUMMY_SP,
                op: AssignOp::Assign,
                variable: target_var_id,
                // call 'target[@@symbol.iterator]' to get the iterator
                value: Box::new(Expr::Call {
                    span: right_span,
                    // callee is 'target[@@symbol.iterator]'
                    callee: Box::new(Callee::Member {
                        span: right_span,
                        // load target from variable
                        object: Expr::VarLoad {
                            span: DUMMY_SP,
                            variable: target_var_id,
                        },
                        // property is [@@symbol.iterator]
                        prop: PropNameOrExpr::PropName(PropName::Symbol(iter_symbol)),
                        // not optchain
                        optional: false,
                    }),
                    // no arguments
                    args: Vec::new(),
                    optional: false,
                }),
            })));

        // variable to store iterator result
        let iterator_result_var_id = VariableId::new();
        // register variable
        self.context.func().variables.insert(
            iterator_result_var_id,
            VariableDesc {
                ty: Type::Any.into(),
                is_heap: false,
            },
        );
        // declare variable
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Let,
            id: iterator_result_var_id,
            ty: Type::Any.into(),
        });

        // create new scope
        self.context.new_scope();

        // enter loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|l| l.into()),
            update: None,
            end_check: None,
        });

        // load iterator
        let load_iterator = Expr::VarLoad {
            span: DUMMY_SP,
            variable: iterator_var_id,
        };
        // call iterator.next
        let mut call_iterator_next = Expr::Call {
            span: right_span,
            callee: Box::new(Callee::Member {
                span: right_span,
                // object is the iterator
                object: load_iterator,
                // property 'next'
                prop: PropNameOrExpr::PropName(PropName::Ident("next".into())),
                optional: false,
            }),
            args: Vec::new(),
            optional: false,
        };
        // await for promise if is await
        if stmt.is_await {
            call_iterator_next = Expr::Await {
                span: right_span,
                future: Box::new(call_iterator_next),
            };
        }

        // store iterator result into variable
        self.context
            .func()
            .stmts
            .push(Stmt::Expr(Box::new(Expr::VarAssign {
                span: right_span,
                op: AssignOp::Assign,
                variable: iterator_result_var_id,
                value: Box::new(call_iterator_next),
            })));
        // check if iterator is done
        let iterator_done = Expr::Member {
            span: right_span,
            object: Box::new(Expr::VarLoad {
                span: right_span,
                variable: iterator_result_var_id,
            }),
            key: PropNameOrExpr::PropName(PropName::Ident("done".into())),
            optional: true,
        };

        // break from loop if iterator done
        self.context.func().stmts.extend_from_slice(&[
            Stmt::If {
                test: Box::new(iterator_done),
            },
            Stmt::Break(None),
            Stmt::EndIf,
        ]);

        // get iterator next value
        let iterator_next_value = Expr::Member {
            span: right_span,
            // from iterator result
            object: Box::new(Expr::VarLoad {
                span: right_span,
                variable: iterator_result_var_id,
            }),
            // property 'value'
            key: PropNameOrExpr::PropName(PropName::Ident("value".into())),
            optional: true,
        };

        // translate assignment to pattern
        match &stmt.left {
            swc::ForHead::Pat(p) => {
                self.translate_pat_assign(
                    p,
                    swc::AssignOp::Assign,
                    iterator_next_value,
                    Type::Any,
                )?;
            }
            swc::ForHead::UsingDecl(u) => {
                // must have only one left-hand side binding
                if u.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        u.span,
                        "Invalid left-hand side in for-in loop: Must have a single binding.",
                    ));
                }

                let decl = &u.decls[0];

                let kind = if u.is_await {
                    VarKind::AwaitUsing
                } else {
                    VarKind::Using
                };

                self.translate_pat_var_decl(
                    u.span,
                    kind,
                    &decl.name,
                    Some(MaybeTranslatedExpr::Translated(
                        iterator_next_value,
                        Type::String,
                    )),
                    None,
                )?;
            }
            swc::ForHead::VarDecl(v) => {
                // must have only one left-hand side binding
                if v.decls.len() != 1 {
                    return Err(Error::syntax_error(
                        v.span,
                        "Invalid left-hand side in for-in loop: Must have a single binding.",
                    ));
                }

                let decl = &v.decls[0];

                self.translate_pat_var_decl(
                    v.span,
                    v.kind.into(),
                    &decl.name,
                    Some(MaybeTranslatedExpr::Translated(
                        iterator_next_value,
                        Type::String,
                    )),
                    None,
                )?;
            }
        };

        // translate loop body
        self.hoist_stmts(core::slice::from_ref(stmt.body.as_ref()).iter())?;
        self.translate_stmt(&stmt.body, None)?;

        // exit loop
        self.context.func().stmts.push(Stmt::EndLoop);

        // close scope
        self.context.end_scope();

        // remove the label
        if let Some(label) = label {
            if !has_old_label {
                self.break_labels.remove(label);
                self.continue_labels.remove(label);
            }
        }

        return Ok(());
    }
}
