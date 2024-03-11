use native_ts_parser::swc_core::common::Span;

use super::{AssignOp, Callee, Expr, PropNameOrExpr, Stmt, UpdateOp};

#[allow(unused)]
pub trait Visitor {
    fn on_new_scope(&mut self) {}
    fn on_close_scope(&mut self) {}

    fn on_block(&mut self) {}
    fn on_if(&mut self) {}
    fn on_else(&mut self) {}
    fn on_loop(&mut self) {}
    fn on_switch(&mut self) {}
    fn on_switch_case(&mut self) {}
    fn on_default_case(&mut self) {}
    fn on_try(&mut self) {}
    fn on_catch(&mut self) {}
    fn on_finally(&mut self) {}

    fn on_return(&mut self, value: &Expr) {}
    fn on_break(&mut self) {}
    fn on_continue(&mut self) {}

    fn on_expr(&mut self, expr: &Expr) {}

    fn on_member(&mut self, object: &Expr, key: &PropNameOrExpr, optional: bool) {}
    fn on_member_assign(
        &mut self,
        assign_op: AssignOp,
        object: &Expr,
        key: &PropNameOrExpr,
        value: &Expr,
    ) {
    }
    fn on_member_update(&mut self, update_op: UpdateOp, object: &Expr, key: &PropNameOrExpr) {}

    fn on_this(&mut self, span: Span) {}

    fn on_call(&mut self, callee: &Callee, args: &[Expr], optional: bool) {}
}

pub fn visit_stmts<V: Visitor>(stmts: &[Stmt], visitor: &mut V) {
    let mut cursor = 0;
    visit_stmts_until(stmts, &mut cursor, |_| false, visitor)
}

fn visit_stmts_until(
    stmts: &[Stmt],
    cursor: &mut usize,
    until: fn(&Stmt) -> bool,
    visitor: &mut dyn Visitor,
) {
    while let Some(stmt) = stmts.get(*cursor) {
        *cursor += 1;

        if until(stmt) {
            return;
        }

        match stmt {
            Stmt::Block { .. } => {
                visitor.on_block();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndBlock => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndBlock => unreachable!(),
            Stmt::Break(_) => {
                visitor.on_break();
            }
            Stmt::Continue(_) => {
                visitor.on_continue();
            }
            Stmt::DeclareClass(_) => {}
            Stmt::DeclareFunction(_) => {}
            Stmt::DeclareGenericClass(_) => {}
            Stmt::DeclareGenericFunction(_) => {}
            Stmt::DeclareGenericInterface(_) => {}
            Stmt::DeclareInterface(_) => {}
            Stmt::DeclareVar { .. } => {}
            Stmt::DropVar(_) => {}
            Stmt::Expr(e) => {
                visit_expr(&e, visitor);
            }
            Stmt::If { test } => {
                visit_expr(&test, visitor);
                visitor.on_if();
                visitor.on_new_scope();

                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndIf => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndIf => unreachable!(),
            Stmt::Else => {
                visitor.on_else();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndElse => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndElse => unreachable!(),
            Stmt::Loop { .. } => {
                visitor.on_loop();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndLoop => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndLoop => unreachable!(),
            Stmt::Return(e) => {
                visit_expr(&e, visitor);
                visitor.on_return(&e);
            }
            Stmt::Switch(e) => {
                visit_expr(&e, visitor);

                visitor.on_switch();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndSwitch => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndSwitch => unreachable!(),
            Stmt::SwitchCase(s) => {
                visit_expr(&s, visitor);

                visitor.on_switch_case();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndSwitchCase => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndSwitchCase => unreachable!(),
            Stmt::DefaultCase => {
                visitor.on_default_case();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndDefaultCase => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndDefaultCase => unreachable!(),
            Stmt::Throw(e) => {
                visit_expr(&e, visitor);
            }
            Stmt::Try => {
                visitor.on_try();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndTry => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndTry => unreachable!(),
            Stmt::Catch(_) => {
                visitor.on_catch();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndCatch => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndCatch => unreachable!(),
            Stmt::Finally => {
                visitor.on_finally();
                visitor.on_new_scope();
                visit_stmts_until(
                    stmts,
                    cursor,
                    |s| match s {
                        Stmt::EndFinally => true,
                        _ => false,
                    },
                    visitor,
                );
                visitor.on_close_scope();
            }
            Stmt::EndFinally => unreachable!(),
        }
    }
}

fn visit_expr(expr: &Expr, visitor: &mut dyn Visitor) {
    match expr {
        Expr::Array { values } => {
            for v in values {
                visit_expr(v, visitor)
            }
        }
        Expr::AssertNonNull(e) => visit_expr(e, visitor),
        Expr::Await(e) => visit_expr(e, visitor),
        Expr::Bigint(_) => {}
        Expr::Bin { left, right, .. } => {
            visit_expr(&left, visitor);
            visit_expr(&right, visitor);
        }
        Expr::Bool(_) => {}
        Expr::Call {
            callee,
            args,
            optional,
        } => {
            for arg in args {
                visit_expr(arg, visitor)
            }
            match callee.as_ref() {
                Callee::Expr(e) => visit_expr(e, visitor),
                Callee::Function(_) => {}
                Callee::Member { object, prop } => {
                    match prop {
                        PropNameOrExpr::PropName(_) => (),
                        PropNameOrExpr::Expr(e, _) => visit_expr(e, visitor),
                    };
                    visit_expr(object, visitor)
                }
                Callee::Super(_) => {}
            }

            visitor.on_call(callee, args, *optional);
        }
        Expr::Cast(e, _) => visit_expr(e, visitor),
        Expr::Closure(_) => (),
        Expr::Function(_) => (),
        Expr::Int(_) => {}
        Expr::Member {
            object,
            key,
            optional,
        } => {
            match key {
                PropNameOrExpr::PropName(_) => {}
                PropNameOrExpr::Expr(e, _) => visit_expr(e, visitor),
            };
            visit_expr(&object, visitor);
            visitor.on_member(object, key, *optional);
        }
        Expr::MemberAssign {
            op,
            object,
            key,
            value,
        } => {
            visit_expr(&value, visitor);
            match key {
                PropNameOrExpr::PropName(_) => {}
                PropNameOrExpr::Expr(e, _) => visit_expr(e, visitor),
            };
            visit_expr(&object, visitor);

            visitor.on_member_assign(*op, object, key, value);
        }
        Expr::MemberUpdate { op, object, key } => {
            match key {
                PropNameOrExpr::PropName(_) => {}
                PropNameOrExpr::Expr(e, _) => visit_expr(e, visitor),
            };
            visit_expr(&object, visitor);

            visitor.on_member_update(*op, object, key);
        }
        Expr::New { class: _, args } => {
            for arg in args {
                visit_expr(arg, visitor)
            }
        }
        Expr::Null => {}
        Expr::Number(_) => {}
        Expr::Object { props } => {
            for (_, e) in props {
                visit_expr(e, visitor)
            }
        }
        Expr::Pop => {}
        Expr::Push(e) => visit_expr(e, visitor),
        Expr::ReadStack => {}
        Expr::Regex() => {}
        Expr::Seq(a, b) => {
            visit_expr(a, visitor);
            visit_expr(b, visitor)
        }
        Expr::String(_) => {}
        Expr::Symbol(_) => {}
        Expr::Ternary { test, left, right } => {
            visit_expr(&test, visitor);
            visit_expr(&left, visitor);
            visit_expr(&right, visitor);
        }
        Expr::This(span) => {
            visitor.on_this(*span);
        }
        Expr::Tuple { values } => {
            for e in values {
                visit_expr(e, visitor)
            }
        }
        Expr::Unary { op: _, value } => visit_expr(&value, visitor),
        Expr::Undefined => {}
        Expr::VarAssign {
            op: _,
            variable: _,
            value,
        } => visit_expr(&value, visitor),
        Expr::VarLoad { .. } => {}
        Expr::VarUpdate { .. } => {}
        Expr::Yield(e) => visit_expr(e, visitor),
    };

    visitor.on_expr(expr);
}
