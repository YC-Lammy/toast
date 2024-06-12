use native_ts_parser::swc_core::common::Spanned;
use native_ts_parser::swc_core::ecma::ast as swc;

use crate::ast_to_hir::expr::MaybeTranslatedExpr;
use crate::error::Error;
use crate::hir::{VarKind, VariableDesc};
use crate::{
    common::VariableId,
    hir::{Expr, Stmt, Type},
};

use super::Result;
use super::Transformer;

impl Transformer {
    /// translate for...in loop
    pub fn translate_for_in_loop_stmt(
        &mut self,
        label: Option<&str>,
        stmt: &swc::ForInStmt,
    ) -> Result<()> {
        let has_old_label = if let Some(label) = label {
            !self.break_labels.insert(label.into()) || !self.continue_labels.insert(label.into())
        } else {
            false
        };

        let right_span = stmt.right.span();

        // translate the iterator
        let (target, _target_ty) = self.translate_expr(&stmt.right, None)?;

        // variable to store iterator result
        let iterator_prop_var_id = VariableId::new();
        // register variable
        self.context.func().variables.insert(
            iterator_prop_var_id,
            VariableDesc {
                ty: Type::String.into(),
                is_heap: false,
            },
        );
        // declare variable
        self.context.func().stmts.push(Stmt::DeclareVar {
            kind: VarKind::Let,
            id: iterator_prop_var_id,
            ty: Type::String.into(),
        });

        // create new scope
        self.context.new_scope();

        // enter loop
        self.context.func().stmts.push(Stmt::ForOfLoop {
            span: stmt.span,
            label: label.map(|l| l.into()),
            binding: iterator_prop_var_id,
            target: Box::new(target),
        });

        // load next propname from stack
        let load_next_prop = Expr::VarLoad {
            span: right_span,
            variable: iterator_prop_var_id,
        };

        // translate assignment to pattern
        match &stmt.left {
            swc::ForHead::Pat(p) => {
                self.translate_pat_assign(p, swc::AssignOp::Assign, load_next_prop, Type::String)?;
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
                        load_next_prop,
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
                        load_next_prop,
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
