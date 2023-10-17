use std::collections::HashSet;

use crate::ir_builder::{ir::IR, IRPackage};

pub fn remove_dead_code_pass(pkg: &mut IRPackage) {
    remove_unread_variables(pkg);
    remove_unused_expr(pkg);
}

pub fn remove_unread_variables(pkg: &mut IRPackage) {
    // varid, readed
    let mut variables = HashSet::new();

    // search for read var
    for ir in &pkg.ir {
        match ir {
            IR::ReadVar(v) => {
                variables.insert(*v);
            }
            _ => {}
        }
    }

    // search for read var
    for (_id, f) in &pkg.functions {
        for ir in &f.ir {
            match ir {
                IR::ReadVar(v) => {
                    variables.insert(*v);
                }
                _ => {}
            }
        }
    }

    let mut removed_vars = Vec::new();

    // remove declare and write var
    for ir in &mut pkg.ir {
        match ir {
            IR::DeclareVar(v) => {
                if variables.get(v).is_none() {
                    // remove
                    removed_vars.push(*v);
                    *ir = IR::Noop;
                }
            }
            IR::WriteVar(v) => {
                if variables.get(v).is_none() {
                    // remove
                    *ir = IR::Noop;
                }
            }
            _ => {}
        }
    }

    for (_id, f) in &mut pkg.functions {
        for ir in &mut f.ir {
            match ir {
                IR::DeclareVar(v) => {
                    if variables.get(v).is_none() {
                        // remove
                        removed_vars.push(*v);
                        *ir = IR::Noop;
                    }
                }
                IR::WriteVar(v) => {
                    if variables.get(v).is_none() {
                        // remove
                        *ir = IR::Noop;
                    }
                }
                _ => {}
            }
        }
    }

    for (_id, func) in &mut pkg.functions {
        for removed in &removed_vars {
            if func.captures.contains(removed) {
                func.captures = func
                    .captures
                    .iter()
                    .map(|v| *v)
                    .filter(|v| v.ne(removed))
                    .collect();
            }
            if func.own_variables.contains(removed) {
                func.own_variables = func
                    .own_variables
                    .iter()
                    .map(|v| *v)
                    .filter(|v| v.ne(removed))
                    .collect();
            }
            if func.heap_variables.contains(removed) {
                func.heap_variables = func
                    .heap_variables
                    .iter()
                    .map(|v| *v)
                    .filter(|v| v.ne(removed))
                    .collect();
            }
        }
    }

    for removed in &removed_vars {
        if pkg.global_variables.contains(removed) {
            pkg.global_variables = pkg
                .global_variables
                .iter()
                .map(|v| *v)
                .filter(|v| v.ne(removed))
                .collect();
        }
        if pkg.heap_variables.contains(removed) {
            pkg.heap_variables = pkg
                .heap_variables
                .iter()
                .map(|v| *v)
                .filter(|v| v.ne(removed))
                .collect();
        }
    }
}

fn remove_unused_expr(pkg: &mut IRPackage) {
    remove_unused_expr_inner(&mut pkg.ir);

    for (_id, f) in &mut pkg.functions {
        remove_unused_expr_inner(&mut f.ir);
    }
}

fn remove_unused_expr_inner(irs: &mut [IR]) {
    let mut i = irs.len() - 1;

    // look backward
    while i > 0 {
        let ir = &irs[i];

        if ir.writes_acc() && !ir.is_call() && !ir.is_await() && !ir.is_yield() {
            let mut has_read = false;

            let mut p = i + 1;
            while p < irs.len() {
                let a = &irs[p];

                if a.reads_acc() {
                    has_read = true;
                    break;
                }
                if a.writes_acc() {
                    break;
                }
                p += 1;
            }

            if !has_read {
                // remove the ir
                irs[i] = IR::Noop;
            }
        }

        i -= 1;
    }
}

impl IR {
    pub fn reads_acc(&self) -> bool {
        match self {
            Self::Add(_) | Self::And(_) => true,
            Self::ObjectPush { array: _ } => true,
            Self::Await | Self::Bang | Self::BitAnd(_) | Self::BitOr(_) | Self::BitXor(_) => true,
            Self::Block { label: _ } => false,
            Self::Break { label: _ } => false,
            Self::BreakIfFalse => true,
            Self::BreakIfIterDone(_) => false,
            Self::Call { .. } => true,
            Self::CallStatic { .. } => false,
            Self::CallVarArgs { .. } => true,
            Self::ObjectCall { .. } => true,
            Self::ObjectCallVarArgs { .. } => true,
            Self::ObjectAssign => true,
            Self::Continue { label: _ } => false,
            Self::CreateArgList(_) => false,
            Self::CreateAsyncIter(_) => true,
            Self::CreateForInIter(_) => true,
            Self::CreateForOfIter(_) => true,
            Self::CreateObject => false,
            Self::CreateRegex {
                pattern: _,
                flag: _,
            } => false,
            Self::Debugger => false,
            Self::DeclareVar(_) => false,
            Self::Delete => true,
            Self::Div(_) => true,
            Self::DropIterator(_) => false,
            Self::DropTemp(_) => false,
            Self::EndBlock => false,
            Self::EndCatch => false,
            Self::EndElse => false,
            Self::EndIf | Self::EndLoop | Self::EndTry | Self::EndTryCatchFinalizer => false,
            Self::EqEq(_) => true,
            Self::EqEqEq(_) => true,
            Self::Exp(_) => true,
            Self::Gt(_) => true,
            Self::GtEq(_) => true,
            Self::If => true,
            Self::IfElse => true,
            Self::In(_) => true,
            Self::InitFunction(_) => false,
            Self::InstanceOf(_) => true,
            Self::IterNext(_) => false,
            Self::LShift(_) => true,
            Self::LoadBigInt(_)
            | Self::LoadBool(_)
            | Self::LoadInt(_)
            | Self::LoadNull
            | Self::LoadNumber(_)
            | Self::LoadString(_)
            | Self::LoadTemp(_)
            | Self::LoadTpl {
                quasis: _,
                args_len: _,
                args: _,
            }
            | Self::LoadUndefined => false,
            Self::Loop { label: _ } => false,
            Self::Lt(_) => true,
            Self::LtEq(_) => true,
            Self::Minus => true,
            Self::Mod(_) => true,
            Self::Mul(_) => true,
            Self::New {
                arg_len: _,
                args: _,
            } => true,
            Self::NonUndefine(_) => true,
            Self::Noop => false,
            Self::NotEq(_) => true,
            Self::NotEqEq(_) => true,
            Self::Nullish(_) => true,
            Self::Or(_) => true,
            Self::Plus => true,
            Self::PushArg(_) => true,
            Self::RShift(_) => true,
            Self::ReadComputed { obj: _ } => true,
            Self::ReadField { key: _ } => true,
            Self::ReadIndex { index: _ } => true,
            Self::ReadParam(_) => false,
            Self::ReadRemainParams { starting_from: _ } => false,
            Self::ReadSuper => false,
            Self::ReadThis => false,
            Self::ReadVar(_) => false,
            Self::Return => true,
            Self::StoreTemp(_) => true,
            Self::Sub(_) => true,
            Self::Throw => true,
            Self::Tilde => true,
            Self::TryCatch => false,
            Self::TypeOf => true,
            Self::WriteComputed {
                obj: _,
                propname: _,
            } => true,
            Self::WriteField { object: _, key: _ } => true,
            Self::WriteIndex {
                object: _,
                index: _,
            } => true,
            Self::WriteVar(_) => true,
            Self::Yield => true,
            Self::ZeroFillRShift(_) => true,
        }
    }

    pub fn writes_acc(&self) -> bool {
        match self {
            Self::Add(_) | Self::And(_) => true,
            Self::Await | Self::Bang | Self::BitAnd(_) | Self::BitOr(_) | Self::BitXor(_) => true,
            Self::Block { label: _ } => false,
            Self::Break { label: _ } => false,
            Self::BreakIfFalse => false,
            Self::BreakIfIterDone(_) => false,
            Self::Call {
                arg_len: _,
                args: _,
                maybe_static: _,
            } => true,
            Self::CallVarArgs { args_array } => true,
            Self::CallStatic {
                func_id: _,
                arg_len: _,
                args: _,
            } => true,
            Self::ObjectCall { .. } => true,
            Self::ObjectCallVarArgs { .. } => true,
            Self::ObjectAssign => false,
            Self::Continue { label: _ } => false,
            Self::CreateArgList(_) => false,
            Self::CreateAsyncIter(_) => false,
            Self::CreateForInIter(_) => false,
            Self::CreateForOfIter(_) => false,
            Self::CreateObject => true,
            Self::CreateRegex {
                pattern: _,
                flag: _,
            } => true,
            Self::Debugger => false,
            Self::DeclareVar(_) => false,
            Self::Delete => true,
            Self::Div(_) => true,
            Self::DropIterator(_) => false,
            Self::DropTemp(_) => false,
            Self::EndBlock => false,
            Self::EndCatch => false,
            Self::EndElse => false,
            Self::EndIf | Self::EndLoop => false,
            Self::EndTry => true,
            Self::EndTryCatchFinalizer => false,
            Self::EqEq(_) => true,
            Self::EqEqEq(_) => true,
            Self::Exp(_) => true,
            Self::Gt(_) => true,
            Self::GtEq(_) => true,
            Self::If => false,
            Self::IfElse => false,
            Self::In(_) => true,
            Self::InitFunction(_) => true,
            Self::InstanceOf(_) => true,
            Self::IterNext(_) => true,
            Self::LShift(_) => true,
            Self::LoadBigInt(_)
            | Self::LoadBool(_)
            | Self::LoadInt(_)
            | Self::LoadNull
            | Self::LoadNumber(_)
            | Self::LoadString(_)
            | Self::LoadTemp(_)
            | Self::LoadTpl {
                quasis: _,
                args_len: _,
                args: _,
            }
            | Self::LoadUndefined => true,
            Self::Loop { label: _ } => false,
            Self::Lt(_) => true,
            Self::LtEq(_) => true,
            Self::Minus => true,
            Self::Mod(_) => true,
            Self::Mul(_) => true,
            Self::New {
                arg_len: _,
                args: _,
            } => true,
            Self::NonUndefine(_) => true,
            Self::Noop => false,
            Self::NotEq(_) => true,
            Self::NotEqEq(_) => true,
            Self::Nullish(_) => true,
            Self::ObjectPush { .. } => false,
            Self::Or(_) => true,
            Self::Plus => true,
            Self::PushArg(_) => false,
            Self::RShift(_) => true,
            Self::ReadComputed { obj: _ } => true,
            Self::ReadField { key: _ } => true,
            Self::ReadIndex { index: _ } => true,
            Self::ReadParam(_) => true,
            Self::ReadRemainParams { starting_from: _ } => true,
            Self::ReadSuper => true,
            Self::ReadThis => true,
            Self::ReadVar(_) => true,
            Self::Return => false,
            Self::StoreTemp(_) => false,
            Self::Sub(_) => true,
            Self::Throw => true,
            Self::Tilde => true,
            Self::TryCatch => false,
            Self::TypeOf => true,
            Self::WriteComputed {
                obj: _,
                propname: _,
            } => false,
            Self::WriteField { object: _, key: _ } => false,
            Self::WriteIndex {
                object: _,
                index: _,
            } => false,
            Self::WriteVar(_) => false,
            Self::Yield => true,
            Self::ZeroFillRShift(_) => true,
        }
    }

    pub fn is_call(&self) -> bool {
        match self {
            IR::Call { .. } | IR::CallStatic { .. } | IR::CallVarArgs { .. } | IR::New { .. } => {
                true
            }
            _ => false,
        }
    }

    pub fn is_await(&self) -> bool {
        match self {
            IR::Await => true,
            _ => false,
        }
    }

    pub fn is_yield(&self) -> bool {
        match self {
            IR::Yield => true,
            _ => false,
        }
    }
}
