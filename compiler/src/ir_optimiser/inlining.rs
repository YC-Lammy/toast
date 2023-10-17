use std::collections::HashMap;

use swc_atoms::JsWord;

use crate::ir_builder::{
    ir::{TempId, IR},
    IRPackage,
};

pub fn resolve_maybe_static_call(pkg: &mut IRPackage) {
    for ir in &mut pkg.ir {
        match ir {
            IR::Call {
                arg_len,
                args,
                maybe_static,
            } => {
                if let Some(id) = maybe_static {
                    *ir = IR::CallStatic {
                        func_id: *id,
                        arg_len: *arg_len,
                        args: *args,
                    };
                }
            }
            _ => {}
        }
    }

    // caller, callee
    let mut need_extend_captures = Vec::new();

    for (funcid, func) in &mut pkg.functions {
        for ir in &mut func.ir {
            match ir {
                IR::Call {
                    arg_len,
                    args,
                    maybe_static,
                } => {
                    if let Some(id) = maybe_static {
                        need_extend_captures.push((*funcid, *id));
                        *ir = IR::CallStatic {
                            func_id: *id,
                            arg_len: *arg_len,
                            args: *args,
                        };
                    }
                }
                _ => {}
            }
        }
    }

    for (caller_id, callee_id) in need_extend_captures {
        let callee = pkg.functions.get(&callee_id).unwrap();
        let captures = callee.captures.clone();
        let caller = pkg.functions.get_mut(&caller_id).unwrap();

        caller.captures.extend_from_slice(&captures);

        let mut parent = caller.parent;

        while let Some(p) = parent {
            let func = pkg.functions.get_mut(&p).unwrap();
            func.captures.extend_from_slice(&captures);

            parent = func.parent;
        }
    }
}

const INLINE_FUNCTION_THRESHOLD: usize = 1024 * 1024;

pub fn inline_static_functions(pkg: &mut IRPackage) {
    let ir = pkg.ir.clone();
    let ir = iniline_static_functions_ir(pkg, ir, true);
    pkg.ir = ir;
}

fn iniline_static_functions_ir(pkg: &IRPackage, mut irs: Vec<IR>, is_async: bool) -> Vec<IR> {
    let origin_len = irs.len();

    let mut arg_lists = HashMap::new();
    let mut inlines = Vec::new();

    let mut i = 0usize;
    for ir in &irs {
        match ir {
            IR::CreateArgList(a) => {
                arg_lists.insert(*a, (i, Vec::new()));
            }
            IR::PushArg(a) => {
                let (_loc, list) = arg_lists.get_mut(a).unwrap();
                list.push(i);
            }
            IR::CallStatic {
                func_id,
                arg_len,
                args,
            } => {
                let (arg_create, arg_list) = arg_lists.remove(&args).unwrap();
                assert!(*arg_len == arg_list.len());

                inlines.push((i, *func_id, arg_create, arg_list));
            }
            _ => {}
        };
        i += 1;
    }

    for (call_loc, func_id, arg_create_loc, args) in inlines {
        let func = pkg.functions.get(&func_id).unwrap();

        if (!is_async && func.is_async) || func.is_generator {
            continue;
        }

        if irs.len() + func.ir.len() > INLINE_FUNCTION_THRESHOLD {
            continue;
        }

        let mut inline_irs = func.ir.clone();

        let offset = irs.len() - origin_len;

        let mut arg_temps = Vec::with_capacity(args.len());
        let return_temp = TempId::new();

        for arg in &args {
            let id = TempId::new();
            arg_temps.push(id);
            irs[*arg + offset] = IR::StoreTemp(id);
        }

        irs[arg_create_loc + offset] = IR::Noop;
        irs[call_loc + offset] = IR::LoadUndefined;

        // process the inlining irs
        let break_label = JsWord::from(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
                .to_string(),
        );

        let mut insert_break = Vec::new();
        let mut insert_remain_params = Vec::new();

        let mut i = 0usize;
        while i < inline_irs.len() {
            let ir = inline_irs[i].clone();

            match ir {
                IR::ReadParam(p) => {
                    if let Some(v) = arg_temps.get(p) {
                        inline_irs[i] = IR::LoadTemp(*v);
                    } else {
                        inline_irs[i] = IR::LoadUndefined;
                    }
                }
                IR::ReadRemainParams { starting_from } => {
                    if starting_from >= arg_temps.len() {
                        inline_irs[i] = IR::CreateObject;
                    } else {
                        insert_remain_params.push((i, starting_from));
                        inline_irs[i] = IR::CreateObject;
                    }
                }

                IR::Return => {
                    inline_irs[i] = IR::StoreTemp(return_temp);
                    insert_break.push(i);
                }
                _ => {}
            };
            i += 1;
        }

        let mut inline_offset = 0;
        for (loc, starting_from) in insert_remain_params {
            let array_temp = TempId::new();
            inline_irs.insert(loc + 1 + inline_offset, IR::StoreTemp(array_temp));
            inline_offset += 1;

            for i in starting_from..arg_temps.len() {
                let temp = arg_temps[i];

                inline_irs.insert(loc + 1 + inline_offset, IR::LoadTemp(temp));
                inline_irs.insert(
                    loc + 1 + inline_offset + 1,
                    IR::ObjectPush { array: array_temp },
                );

                inline_offset += 2;
            }

            inline_irs.insert(loc + 1 + inline_offset, IR::DropTemp(array_temp));
            inline_offset += 1;
        }

        for loc in insert_break {
            inline_irs.insert(
                loc + 1 + inline_offset,
                IR::Break {
                    label: Some(break_label.clone()),
                },
            );
            inline_offset += 1;
        }

        inline_irs.insert(0, IR::StoreTemp(return_temp));
        inline_irs.insert(
            1,
            IR::Block {
                label: break_label.clone(),
            },
        );
        inline_irs.push(IR::EndBlock);

        for t in arg_temps {
            inline_irs.push(IR::DropTemp(t));
        }

        inline_irs.push(IR::LoadTemp(return_temp));
        inline_irs.push(IR::DropTemp(return_temp));

        let copy_length = irs.len() - (call_loc + 1 + offset);

        unsafe {
            irs.reserve(inline_irs.len());
            irs.set_len(irs.len() + inline_irs.len());

            core::ptr::copy(
                &irs[call_loc + 1 + offset],
                &mut irs[call_loc + 1 + offset + inline_irs.len()],
                copy_length,
            );
            core::ptr::copy(
                inline_irs.as_ptr(),
                &mut irs[call_loc + 1 + offset],
                inline_irs.len(),
            );

            inline_irs.set_len(0);
        }
    }

    return irs;
}
