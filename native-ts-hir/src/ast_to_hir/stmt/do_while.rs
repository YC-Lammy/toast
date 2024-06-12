use native_ts_parser::swc_core::common::DUMMY_SP;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::hir::{BinOp, UnaryOp, VarKind, VariableDesc};
use crate::{
    common::VariableId,
    hir::{Expr, Stmt, Type},
};

use super::Result;
use super::Transformer;

impl Transformer {
    pub fn translate_do_while_stmt(
        &mut self,
        label: Option<&str>,
        stmt: &swc::DoWhileStmt,
    ) -> Result<()> {
        let (has_old_break, has_old_continue) = if let Some(label) = label {
            (
                !self.break_labels.insert(label.into()),
                !self.continue_labels.insert(label.into()),
            )
        } else {
            (false, false)
        };

        // a variable to indicate first loop
        let is_first_loop_var_id = VariableId::new();
        // register variable
        self.context.func().variables.insert(
            is_first_loop_var_id,
            VariableDesc {
                ty: Type::Bool.into(),
                is_heap: false,
            },
        );
        // declare variable
        self.context.func().stmts.extend_from_slice(&[
            Stmt::DeclareVar {
                kind: VarKind::Let,
                id: is_first_loop_var_id,
                ty: Type::Bool.into(),
            },
            Stmt::Expr(Box::new(Expr::VarAssign {
                span: stmt.span,
                op: crate::hir::AssignOp::Assign,
                variable: is_first_loop_var_id,
                value: Box::new(Expr::Bool(true)),
            })),
        ]);

        let (test, _test_ty) = self.translate_expr(&stmt.test, None)?;

        // new scope
        self.context.new_scope();

        // enter loop
        self.context.func().stmts.push(Stmt::Loop {
            label: label.map(|s| s.into()),
            update: None,
            end_check: None,
        });

        // if not first and not test, break
        self.context.func().stmts.extend_from_slice(&[
            Stmt::If {
                test: Box::new(Expr::Bin {
                    span: DUMMY_SP,
                    op: BinOp::And,
                    // not first
                    left: Box::new(Expr::Unary {
                        span: DUMMY_SP,
                        op: UnaryOp::LogicalNot,
                        value: Box::new(Expr::VarLoad {
                            span: DUMMY_SP,
                            variable: is_first_loop_var_id,
                        }),
                    }),
                    // not test
                    right: Box::new(Expr::Unary {
                        span: DUMMY_SP,
                        op: UnaryOp::LogicalNot,
                        value: Box::new(test),
                    }),
                }),
            },
            Stmt::Break(None),
            Stmt::EndIf,
            // update flag
            Stmt::Expr(Box::new(Expr::VarAssign {
                span: DUMMY_SP,
                op: crate::hir::AssignOp::Assign,
                variable: is_first_loop_var_id,
                value: Box::new(Expr::Bool(false)),
            })),
        ]);

        // translate body
        self.hoist_stmts(core::slice::from_ref(stmt.body.as_ref()).iter())?;
        self.translate_stmt(&stmt.body, None)?;

        // end loop
        self.context.func().stmts.push(Stmt::EndLoop);

        // end scope
        self.context.end_scope();

        if let Some(label) = label{
            if !has_old_break{
                self.break_labels.remove(label);
            }
            if !has_old_continue{
                self.continue_labels.remove(label);
            }
        }

        return Ok(());
    }
}
