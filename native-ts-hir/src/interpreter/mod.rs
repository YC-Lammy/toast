use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use parking_lot::RwLock;

use crate::ast::*;
use crate::common::{ClassId, EnumId, FunctionId, InterfaceId, ModuleId, VariableId};
use crate::symbol_table::SymbolTable;
use crate::PropName;
use crate::Symbol;

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Int(i32),
    Number(f64),
    /// loads an i128
    Bigint(i128),
    /// loads a string
    String(String),
    Symbol(Symbol),
    Regex(),
    Function(FunctionId),
    Clousure {
        id: FunctionId,
        captures: Arc<HashMap<VariableId, Arc<RwLock<Value>>>>,
    },
    Array(Arc<RwLock<Vec<Value>>>),
    Tuple(Arc<RwLock<Vec<Value>>>),
    Interface {
        id: InterfaceId,
        value: Box<Value>,
    },
    Union {
        ty: Box<[Type]>,
        value: Box<Value>,
    },
    Any(Box<Value>),
    AnyObject(Box<Value>),
    Object {
        id: ClassId,
        values: Arc<[(PropName, Value)]>,
    },
    DynObject {
        values: Arc<[(PropName, Value)]>,
    },
    Enum {
        id: EnumId,
        variant: usize,
    },
}

#[derive(Debug)]
enum StmtResult {
    Ok,
    Return(Value),
    Break(Option<String>),
    Continue(Option<String>),
    Error(Value),
}

struct Context {
    variables: HashMap<VariableId, Value>,
    heap_variables: HashMap<VariableId, Arc<RwLock<Value>>>,
    using: Vec<VariableId>,
    await_using: Vec<VariableId>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            heap_variables: HashMap::new(),
            using: Vec::new(),
            await_using: Vec::new(),
        }
    }
    pub fn declare(&mut self, id: VariableId, is_heap: bool) {
        if is_heap {
            self.heap_variables
                .insert(id, Arc::new(RwLock::new(Value::Undefined)));
        } else {
            self.variables.insert(id, Value::Undefined);
        }
    }

    pub fn read(&self, id: VariableId) -> Value {
        if let Some(v) = self.variables.get(&id) {
            return v.clone();
        }
        if let Some(v) = self.heap_variables.get(&id) {
            return v.read().clone();
        }
        panic!("invalid variable id")
    }
    pub fn write(&mut self, id: VariableId, value: Value) {
        if let Some(v) = self.variables.get_mut(&id) {
            *v = value;
            return;
        }
        if let Some(v) = self.heap_variables.get(&id) {
            *v.write() = value;
            return;
        }
        panic!("invalid variable id")
    }

    pub fn contains(&self, id: VariableId) -> bool {
        self.variables.contains_key(&id) || self.heap_variables.contains_key(&id)
    }

    pub fn remove(&mut self, id: VariableId) -> bool {
        if self.variables.remove(&id).is_none() {
            return self.variables.remove(&id).is_some();
        }

        return true;
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }
    pub fn run(&self, program: &Program) -> Result<Value, Value> {
        let mut runner = InterpreterRunning {
            table: &program.table,
            modules: &program.modules,

            ran_modules: HashSet::new(),

            this: Value::Undefined,
            pc: Value::Undefined,
            stack: Vec::new(),
            context: Context::new(),
        };

        runner.run_module_with_dependencies(program.entry)
    }
}

struct InterpreterRunning<'a> {
    table: &'a SymbolTable,
    modules: &'a HashMap<ModuleId, Module>,

    ran_modules: HashSet<ModuleId>,

    this: Value,
    pc: Value,
    stack: Vec<Value>,

    context: Context,
}

impl<'a> InterpreterRunning<'a> {
    fn run_module_with_dependencies(&mut self, id: ModuleId) -> Result<Value, Value> {
        let module = self.modules.get(&id).expect("invalid module");

        for dep in &module.dependencies {
            if self.ran_modules.insert(*dep) {
                self.run_module_with_dependencies(*dep)?;
            }
        }

        self.run_module(module)
    }

    fn run_module(&mut self, module: &Module) -> Result<Value, Value> {
        let main = self
            .table
            .functions
            .get(&module.main_function)
            .expect("invalid function");

        self.run_function(
            main,
            Value::Any(Box::new(Value::Undefined)),
            &[],
            Context::new(),
        )?;

        return Ok(self.pc.clone());
    }

    fn run_function(
        &mut self,
        func: &Function,
        this: Value,
        args: &[Value],
        new_context: Context,
    ) -> Result<Value, Value> {
        let old_context = core::mem::replace(&mut self.context, new_context);

        // assert this type
        self.assert_type(&this, &func.this_ty);

        assert!(func.params.len() == args.len());

        for (id, desc) in &func.variables {
            self.context.declare(*id, desc.is_heap);
        }

        for i in 0..args.len() {
            let param = &func.params[i];
            self.assert_type(&args[i], &param.ty);

            self.context.write(param.id, args[i].clone());
        }

        for (id, _) in &func.captures {
            if !self.context.heap_variables.contains_key(id) {
                self.context.heap_variables.insert(
                    *id,
                    old_context
                        .heap_variables
                        .get(id)
                        .expect("missing capture")
                        .clone(),
                );
            }
        }

        let mut cursor = 0;

        match self.run_stmts(&func.stmts, &mut cursor) {
            StmtResult::Ok => {
                self.dispose_context();
                // restore context
                self.context = old_context;
            }
            StmtResult::Continue(_) => panic!("invalid continue"),
            StmtResult::Break(_) => panic!("invalid break"),
            StmtResult::Return(r) => {
                self.dispose_context();
                // restore context
                self.context = old_context;
                return Ok(r);
            }
            StmtResult::Error(e) => {
                self.dispose_context();
                // restore context
                self.context = old_context;
                return Err(e);
            }
        }

        return Ok(Value::Undefined);
    }

    fn dispose_context(&mut self) {
        for i in 0..self.context.using.len() {
            let u = self.context.using[i];
            let v = self
                .context
                .variables
                .get(&u)
                .expect("invalid variable")
                .clone();
            if let Some(dispose) = self.get_property(&v, &PropName::Symbol(Symbol::Dispose)) {
                match dispose {
                    Value::Function(id) => {
                        let func = self.table.functions.get(&id).expect("invalid function");

                        self.run_function(func, v, &[], Context::new());
                    }
                    Value::Clousure { id, captures } => {
                        let func = self.table.functions.get(&id).expect("invalid function");

                        let mut ctx = Context::new();
                        for (vid, cap) in captures.iter() {
                            ctx.heap_variables.insert(*vid, cap.clone());
                        }

                        self.run_function(func, v, &[], ctx);
                    }
                    _ => panic!("callee is not a function"),
                }
            }
        }
    }

    fn end_scope<A: Fn(&Stmt) -> bool, B: Fn(&Stmt) -> bool>(
        &mut self,
        stmts: &[Stmt],
        cursor: &mut usize,
        open_scope: A,
        end_scope: B,
    ) {
        // number of scopes opened
        let mut i = 0;
        loop {
            let s = &stmts[*cursor];
            *cursor += 1;

            if end_scope(s) {
                // no interior scope is present
                if i == 0 {
                    break;
                } else {
                    // decrement interier scope
                    i -= 1;
                }
            };

            // opens an interier scope
            if open_scope(s) {
                i += 1;
            }
        }
    }

    fn run_stmts(&mut self, stmts: &[Stmt], cursor: &mut usize) -> StmtResult {
        self.run_stmts_until(stmts, cursor, |_| false)
    }

    #[inline(never)]
    fn run_stmts_until(
        &mut self,
        stmts: &[Stmt],
        cursor: &mut usize,
        until: fn(&Stmt) -> bool,
    ) -> StmtResult {
        while *cursor < stmts.len() {
            let stmt = &stmts[*cursor];
            *cursor += 1;

            if until(stmt) {
                return StmtResult::Ok;
            }

            match stmt {
                Stmt::Block { label } => {
                    let re = self.run_stmts_until(stmts, cursor, |s| {
                        if let Stmt::EndBlock = s {
                            true
                        } else {
                            false
                        }
                    });

                    match re {
                        StmtResult::Break(l) => {
                            if let Some(a) = &l {
                                // the label does not match, fallout
                                if a != label {
                                    return StmtResult::Break(l);
                                } else {
                                    // a break occoured, loop until end is reached

                                    // counter for block scope opened
                                    let mut i = 0;
                                    loop {
                                        let s = &stmts[*cursor];
                                        *cursor += 1;

                                        if let Stmt::EndBlock = s {
                                            // no interior scope is present
                                            if i == 0 {
                                                break;
                                            } else {
                                                // decrement interier scope
                                                i -= 1;
                                            }
                                        };

                                        // opens an interier scope
                                        if let Stmt::Block { .. } = s {
                                            i += 1;
                                        }
                                    }
                                }
                            } else {
                                return StmtResult::Break(l);
                            }
                        }
                        StmtResult::Continue(_) => return re,
                        StmtResult::Return(_) => return re,
                        StmtResult::Error(_) => return re,
                        StmtResult::Ok => (),
                    };
                }
                Stmt::EndBlock => unreachable!(),
                Stmt::Break(label) => return StmtResult::Break(label.clone()),
                Stmt::Continue(label) => return StmtResult::Continue(label.clone()),
                Stmt::Return(r) => {
                    let re = self.run_expr(&r);

                    match re {
                        Ok(v) => return StmtResult::Return(v),
                        Err(e) => return StmtResult::Error(e),
                    }
                }
                Stmt::Throw(v) => {
                    let re = self.run_expr(&v);
                    let v = match re {
                        Ok(v) => v,
                        Err(v) => v,
                    };

                    return StmtResult::Error(v);
                }
                Stmt::DeclareClass(_)
                | Stmt::DeclareFunction(_)
                | Stmt::DeclareGenericClass(_)
                | Stmt::DeclareGenericFunction(_)
                | Stmt::DeclareGenericInterface(_)
                | Stmt::DeclareInterface(_) => {
                    // do nothing
                }
                Stmt::DeclareVar { kind, id, ty: _ } => {
                    // check if variable is already inserted
                    assert!(self.context.contains(*id));

                    // regester for dispose
                    match kind {
                        VarKind::Using => {
                            self.context.using.push(*id);
                        }
                        VarKind::AwaitUsing => {
                            self.context.await_using.push(*id);
                        }
                        _ => {}
                    };
                }
                Stmt::DropVar(v) => {
                    assert!(self.context.remove(*v));
                }
                Stmt::If { test } => {
                    let value = match self.run_expr(&test) {
                        Ok(v) => v,
                        Err(e) => return StmtResult::Error(e),
                    };

                    self.assert_type(&value, &Type::Bool);

                    let mut is_if_ran = false;

                    // test value is true
                    if self.to_bool(&value) {
                        is_if_ran = true;

                        // run statement in clause
                        let re = self.run_stmts_until(stmts, cursor, |s| {
                            if let Stmt::EndIf = s {
                                true
                            } else {
                                false
                            }
                        });

                        match re {
                            StmtResult::Ok => {}
                            // does not accept breaks
                            _ => return re,
                        }
                    } else {
                        // loop until end if is reached
                        self.end_scope(
                            stmts,
                            cursor,
                            |s| match s {
                                Stmt::If { .. } => true,
                                _ => false,
                            },
                            |s| match s {
                                Stmt::EndIf => true,
                                _ => false,
                            },
                        );
                    };

                    // run the else clause
                    if let Some(Stmt::Else) = stmts.get(*cursor) {
                        *cursor += 1;

                        // the revious conditions are not met, else clause should run
                        if !is_if_ran {
                            let re = self.run_stmts_until(stmts, cursor, |s| {
                                if let Stmt::EndElse = s {
                                    true
                                } else {
                                    false
                                }
                            });
                            match re {
                                StmtResult::Ok => {}
                                _ => return re,
                            }
                        } else {
                            // else clause should not run
                            // loop until end else is reached
                            self.end_scope(
                                stmts,
                                cursor,
                                |s| match s {
                                    Stmt::Else { .. } => true,
                                    _ => false,
                                },
                                |s| match s {
                                    Stmt::EndElse => true,
                                    _ => false,
                                },
                            );
                        }
                    };
                }
                Stmt::EndIf => unreachable!(),
                Stmt::Else => unreachable!(),
                Stmt::EndElse => unreachable!(),
                // expression statement
                Stmt::Expr(e) => {
                    // run expression
                    match self.run_expr(&e) {
                        // ok
                        Ok(v) => {
                            // set pc to value
                            self.pc = v;
                        }
                        // retunr error
                        Err(e) => return StmtResult::Error(e),
                    };
                }
                Stmt::Loop { label } => {
                    // the loop
                    loop {
                        let mut new_cursor = *cursor;

                        // execute statements within loop
                        let re = self.run_stmts_until(stmts, &mut new_cursor, |s| {
                            if let Stmt::EndLoop = s {
                                true
                            } else {
                                false
                            }
                        });

                        println!("{:#?}", re);

                        match re {
                            StmtResult::Break(l) => {
                                // if no label is specified, break anyways
                                if l.is_none() || &l == label {
                                    break;
                                } else {
                                    // label not match, break
                                    return StmtResult::Break(l);
                                }
                            }
                            StmtResult::Continue(l) => {
                                // label is not specidied or is none
                                if l.is_none() || &l == label {
                                    continue;
                                } else {
                                    return StmtResult::Continue(l);
                                }
                            }
                            StmtResult::Return(_) => return re,
                            StmtResult::Error(_) => return re,
                            StmtResult::Ok => (),
                        };
                    }

                    // loop until endloop is reached

                    // counter for loop scope opened
                    let mut i = 0;
                    loop {
                        let s = &stmts[*cursor];
                        *cursor += 1;

                        if let Stmt::EndLoop = s {
                            // no interior scope is present
                            if i == 0 {
                                break;
                            } else {
                                // decrement interier scope
                                i -= 1;
                            }
                        };

                        // opens an interier scope
                        if let Stmt::Loop { .. } = s {
                            i += 1;
                        }
                    }
                }
                Stmt::EndLoop => unreachable!(),
                Stmt::Try => {
                    let re = self.run_stmts_until(stmts, cursor, |s| {
                        if let Stmt::EndTry = s {
                            true
                        } else {
                            false
                        }
                    });

                    let mut error = None;
                    match re {
                        StmtResult::Ok => {}
                        StmtResult::Error(e) => {
                            error = Some(e);
                        }
                        _ => return re,
                    };

                    // an error means that endtry was not reached
                    if error.is_some() {
                        // loop until endtry is reached
                        self.end_scope(
                            stmts,
                            cursor,
                            |s| match s {
                                Stmt::Try => true,
                                _ => false,
                            },
                            |s| match s {
                                Stmt::EndTry => true,
                                _ => false,
                            },
                        );
                    }

                    let mut error_in_catch = None;

                    if let Some(Stmt::Catch(vid)) = stmts.get(*cursor) {
                        *cursor += 1;

                        // run the catch clause if error occoured
                        if let Some(error) = error {
                            let error = self.cast(&error, &Type::Any);
                            // insert binding
                            self.context.declare(*vid, false);
                            self.context.write(*vid, error);

                            let re = self.run_stmts_until(stmts, cursor, |s| {
                                if let Stmt::EndCatch = s {
                                    true
                                } else {
                                    false
                                }
                            });

                            match re {
                                StmtResult::Ok => {}
                                StmtResult::Error(e) => {
                                    error_in_catch = Some(e);
                                }
                                _ => return re,
                            }
                        } else {
                            // no error occoured, end catch
                            self.end_scope(
                                stmts,
                                cursor,
                                |s| match s {
                                    Stmt::Catch(_) => true,
                                    _ => false,
                                },
                                |s| match s {
                                    Stmt::EndCatch => true,
                                    _ => false,
                                },
                            );
                        }
                    }

                    // run the finaliser no matter what
                    if let Some(Stmt::Finally) = stmts.get(*cursor) {
                        *cursor += 1;
                        let re = self.run_stmts_until(stmts, cursor, |s| {
                            if let Stmt::EndFinally = s {
                                true
                            } else {
                                false
                            }
                        });

                        match re {
                            StmtResult::Ok => {}
                            _ => return re,
                        }
                    }

                    // an error is thrown when catching
                    if let Some(e) = error_in_catch {
                        return StmtResult::Error(e);
                    };
                }
                Stmt::EndTry => unreachable!(),
                Stmt::Catch(_) => unreachable!(),
                Stmt::EndCatch => unreachable!(),
                Stmt::Finally => unreachable!(),
                Stmt::EndFinally => unreachable!(),
                Stmt::Switch(value) => {
                    // run the expression
                    let value = match self.run_expr(&value) {
                        Ok(v) => v,
                        Err(e) => return StmtResult::Error(e),
                    };

                    let mut is_case_matched = false;

                    // run all the switch cases
                    while let Some(Stmt::SwitchCase(test)) = stmts.get(*cursor) {
                        *cursor += 1;

                        // run the expression
                        let test = match self.run_expr(&test) {
                            Ok(v) => v,
                            Err(e) => return StmtResult::Error(e),
                        };

                        // if no case was matched, try to match case
                        if !is_case_matched {
                            is_case_matched = self.strict_equal(&value, &test);
                        };

                        // if case is matched, execute statements
                        // this may be an effect of fallthrough
                        if is_case_matched {
                            let re = self.run_stmts_until(stmts, cursor, |s| {
                                if let Stmt::EndSwitchCase = s {
                                    true
                                } else {
                                    false
                                }
                            });

                            match re {
                                StmtResult::Break(label) => {
                                    // break from switch
                                    if label.is_none() {
                                        break;
                                    } else {
                                        return StmtResult::Break(label);
                                    }
                                }
                                StmtResult::Ok => {}
                                _ => return re,
                            };
                        } else {
                            // case is not matched, loop until end switch case is reached

                            self.end_scope(
                                stmts,
                                cursor,
                                |s| match s {
                                    Stmt::SwitchCase(_) => true,
                                    _ => false,
                                },
                                |s| match s {
                                    Stmt::EndSwitchCase => true,
                                    _ => false,
                                },
                            );
                        }
                    }

                    // run the default case if no case is matched
                    if let Some(Stmt::DefaultCase) = stmts.get(*cursor) {
                        *cursor += 1;

                        if !is_case_matched {
                            let re = self.run_stmts_until(stmts, cursor, |s| {
                                if let Stmt::EndDefaultCase = s {
                                    true
                                } else {
                                    false
                                }
                            });

                            match re {
                                StmtResult::Ok => {}
                                _ => return re,
                            }
                        } else {
                            self.end_scope(
                                stmts,
                                cursor,
                                |s| match s {
                                    Stmt::DefaultCase => true,
                                    _ => false,
                                },
                                |s| match s {
                                    Stmt::EndDefaultCase => true,
                                    _ => false,
                                },
                            );
                        }
                    };

                    // loop until switch end

                    self.end_scope(
                        stmts,
                        cursor,
                        |s| match s {
                            Stmt::Switch(_) => true,
                            _ => false,
                        },
                        |s| match s {
                            Stmt::EndSwitch => true,
                            _ => false,
                        },
                    );
                }
                Stmt::EndSwitch => unreachable!(),
                Stmt::SwitchCase(_) => unreachable!(),
                Stmt::EndSwitchCase => unreachable!(),
                Stmt::DefaultCase => unreachable!(),
                Stmt::EndDefaultCase => unreachable!(),
            };
        }

        return StmtResult::Ok;
    }

    fn run_expr(&mut self, expr: &Expr) -> Result<Value, Value> {
        match expr {
            Expr::Undefined => Ok(Value::Undefined),
            Expr::Null => Ok(Value::Null),
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::Number(f) => Ok(Value::Number(*f)),
            Expr::Bigint(b) => Ok(Value::Bigint(*b)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Symbol(s) => Ok(Value::Symbol(*s)),
            Expr::Regex() => Ok(Value::Regex()),
            Expr::Function(f) => Ok(Value::Function(*f)),
            Expr::This(_) => Ok(self.this.clone()),
            Expr::Array { values } => {
                let mut array = Vec::new();
                for v in values {
                    array.push(self.run_expr(v)?);
                }
                Ok(Value::Array(Arc::new(RwLock::new(array))))
            }
            Expr::Tuple { values } => {
                let mut tuple = Vec::new();
                for v in values {
                    tuple.push(self.run_expr(v)?);
                }
                Ok(Value::Tuple(Arc::new(RwLock::new(tuple))))
            }
            Expr::Object { props } => {
                let mut obj = Vec::new();
                for (p, e) in props {
                    let v = self.run_expr(e)?;
                    obj.push((p.clone(), v));
                }
                Ok(Value::DynObject { values: obj.into() })
            }
            Expr::New { class, args } => {
                todo!()
            }
            Expr::Call {
                callee,
                args,
                optional,
            } => {
                let (this, mut callee) = match callee.as_ref() {
                    Callee::Expr(e) => (self.this.clone(), Some(self.run_expr(e)?)),
                    Callee::Function(f) => (self.this.clone(), Some(Value::Function(*f))),
                    Callee::Member { object, prop } => {
                        let obj = self.run_expr(object)?;
                        let method = match prop {
                            PropNameOrExpr::PropName(p) => self.get_property(&obj, p),
                            PropNameOrExpr::Expr(e, _) => {
                                let p = self.run_expr(e)?;
                                self.get_property_expr(&obj, &p)?
                            }
                        };
                        (obj, method)
                    }
                    Callee::Super(s) => {
                        todo!()
                    }
                };

                let mut arguments = Vec::new();

                for arg in args {
                    arguments.push(self.run_expr(arg)?);
                }

                if *optional {
                    if let Some(c) = &callee {
                        match self.assert_not_null(c.clone()) {
                            Ok(v) => callee = Some(v),
                            Err(_) => return Ok(Value::Undefined),
                        }
                    } else {
                        return Ok(Value::Undefined);
                    }
                }

                match callee {
                    Some(Value::Function(id)) => {
                        let func = self.table.functions.get(&id).expect("invalid function");

                        return self.run_function(func, this, &arguments, Context::new());
                    }
                    Some(Value::Clousure { id, captures }) => {
                        let func = self.table.functions.get(&id).expect("invalid function");

                        let mut ctx = Context::new();
                        for (vid, cap) in captures.iter() {
                            ctx.heap_variables.insert(*vid, cap.clone());
                        }

                        return self.run_function(func, this, &arguments, ctx);
                    }
                    _ => panic!("callee is not a function"),
                }
            }
            Expr::Push(v) => {
                let v = self.run_expr(v)?;
                self.stack.push(v.clone());
                return Ok(v);
            }
            Expr::ReadStack => Ok(self.stack.last().expect("stack underflow").clone()),
            Expr::Pop => Ok(self.stack.pop().expect("stack underflow")),
            Expr::Member {
                object,
                key,
                optional,
            } => {
                let obj = self.run_expr(&object)?;

                let v = match key {
                    PropNameOrExpr::PropName(p) => self.get_property(&obj, p),
                    PropNameOrExpr::Expr(e, _) => {
                        let key = self.run_expr(e)?;
                        self.get_property_expr(&obj, &key)?
                    }
                };

                if let Some(v) = v {
                    Ok(v)
                } else {
                    if *optional {
                        Ok(Value::Undefined)
                    } else {
                        panic!("missing property")
                    }
                }
            }
            Expr::MemberAssign {
                op,
                object,
                key,
                value,
            } => {
                let obj = self.run_expr(&object)?;
                let value = self.run_expr(&value)?;

                if op == &AssignOp::Assign {
                    match key {
                        PropNameOrExpr::PropName(p) => self.set_property(&obj, p, value.clone()),
                        PropNameOrExpr::Expr(e, _) => {
                            let key = self.run_expr(e)?;
                            self.set_property_expr(&obj, key, value.clone());
                        }
                    };
                    return Ok(value);
                }

                enum PorV {
                    P(PropName),
                    V(Value),
                }

                let key = match &key {
                    PropNameOrExpr::PropName(p) => PorV::P(p.clone()),
                    PropNameOrExpr::Expr(e, _) => {
                        let key = self.run_expr(e)?;
                        PorV::V(key)
                    }
                };

                let old = match &key {
                    PorV::P(key) => self.get_property(&obj, &key),
                    PorV::V(v) => self.get_property_expr(&obj, &v)?,
                }
                .expect("invalid property");

                let value = match op {
                    AssignOp::Assign => unreachable!(),
                    AssignOp::AddAssign => self.add(old, value),
                    AssignOp::SubAssign => self.sub(old, value),
                    AssignOp::MulAssign => self.mul(old, value),
                    AssignOp::DivAssign => self.div(old, value),
                    AssignOp::ExpAssign => self.exp(old, value)?,
                    AssignOp::ModAssign => self.rem(old, value),
                    AssignOp::LShiftAssign => self.lshift(old, value),
                    AssignOp::RShiftAssign => self.rshift(old, value),
                    AssignOp::ZeroFillRShiftAssign => self.zero_fill_rshift(old, value),
                    AssignOp::BitAndAssign => self.bitand(old, value),
                    AssignOp::BitOrAssign => self.bitor(old, value),
                    AssignOp::BitXorAssign => self.bixor(old, value),
                    AssignOp::OrAssign => self.or(old, value),
                    AssignOp::AndAssign => self.and(old, value),
                    AssignOp::NullishAssign => self.nullish(old, value),
                };

                match key {
                    PorV::P(p) => self.set_property(&obj, &p, value.clone()),
                    PorV::V(key) => {
                        self.set_property_expr(&obj, key, value.clone());
                    }
                };

                return Ok(value);
            }
            Expr::MemberUpdate { op, object, key } => {
                let obj = self.run_expr(&object)?;

                enum PorV {
                    P(PropName),
                    V(Value),
                }

                let key = match key {
                    PropNameOrExpr::PropName(p) => PorV::P(p.clone()),
                    PropNameOrExpr::Expr(e, _) => {
                        let key = self.run_expr(e)?;
                        PorV::V(key)
                    }
                };

                let old = match &key {
                    PorV::P(key) => self.get_property(&obj, &key),
                    PorV::V(v) => self.get_property_expr(&obj, &v)?,
                }
                .expect("invalid property");

                let (rv, nv) = match op {
                    UpdateOp::PrefixAdd => match old {
                        Value::Int(i) => (Value::Int(i + 1), Value::Int(i + 1)),
                        Value::Number(n) => (Value::Number(n + 1.0), Value::Number(n + 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n + 1), Value::Bigint(n + 1)),
                        _ => panic!(),
                    },
                    UpdateOp::PrefixSub => match old {
                        Value::Int(i) => (Value::Int(i - 1), Value::Int(i - 1)),
                        Value::Number(n) => (Value::Number(n - 1.0), Value::Number(n - 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n - 1), Value::Bigint(n - 1)),
                        _ => panic!(),
                    },
                    UpdateOp::SuffixAdd => match old {
                        Value::Int(i) => (Value::Int(i), Value::Int(i + 1)),
                        Value::Number(n) => (Value::Number(n), Value::Number(n + 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n), Value::Bigint(n + 1)),
                        _ => panic!(),
                    },
                    UpdateOp::SuffixSub => match old {
                        Value::Int(i) => (Value::Int(i), Value::Int(i - 1)),
                        Value::Number(n) => (Value::Number(n), Value::Number(n - 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n), Value::Bigint(n - 1)),
                        _ => panic!(),
                    },
                };

                match key {
                    PorV::P(p) => self.set_property(&obj, &p, nv),
                    PorV::V(key) => {
                        self.set_property_expr(&obj, key, nv);
                    }
                };

                return Ok(rv);
            }
            Expr::VarAssign {
                op,
                variable,
                value,
            } => {
                let value = self.run_expr(&value)?;

                if let AssignOp::Assign = op {
                    self.context.write(*variable, value.clone());
                    return Ok(value);
                }

                let old = self.context.read(*variable);

                let value = match op {
                    AssignOp::Assign => unreachable!(),
                    AssignOp::AddAssign => self.add(old, value),
                    AssignOp::SubAssign => self.sub(old, value),
                    AssignOp::MulAssign => self.mul(old, value),
                    AssignOp::DivAssign => self.div(old, value),
                    AssignOp::ExpAssign => self.exp(old, value)?,
                    AssignOp::ModAssign => self.rem(old, value),
                    AssignOp::LShiftAssign => self.lshift(old, value),
                    AssignOp::RShiftAssign => self.rshift(old, value),
                    AssignOp::ZeroFillRShiftAssign => self.zero_fill_rshift(old, value),
                    AssignOp::BitAndAssign => self.bitand(old, value),
                    AssignOp::BitOrAssign => self.bitor(old, value),
                    AssignOp::BitXorAssign => self.bixor(old, value),
                    AssignOp::OrAssign => self.or(old, value),
                    AssignOp::AndAssign => self.and(old, value),
                    AssignOp::NullishAssign => self.nullish(old, value),
                };

                self.context.write(*variable, value.clone());

                return Ok(value);
            }
            Expr::VarLoad { span: _, variable } => return Ok(self.context.read(*variable)),
            Expr::VarUpdate { op, variable } => {
                let old = self.context.read(*variable);

                let (rv, nv) = match op {
                    UpdateOp::PrefixAdd => match old {
                        Value::Int(i) => (Value::Int(i + 1), Value::Int(i + 1)),
                        Value::Number(n) => (Value::Number(n + 1.0), Value::Number(n + 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n + 1), Value::Bigint(n + 1)),
                        _ => panic!(),
                    },
                    UpdateOp::PrefixSub => match old {
                        Value::Int(i) => (Value::Int(i - 1), Value::Int(i - 1)),
                        Value::Number(n) => (Value::Number(n - 1.0), Value::Number(n - 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n - 1), Value::Bigint(n - 1)),
                        _ => panic!(),
                    },
                    UpdateOp::SuffixAdd => match old {
                        Value::Int(i) => (Value::Int(i), Value::Int(i + 1)),
                        Value::Number(n) => (Value::Number(n), Value::Number(n + 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n), Value::Bigint(n + 1)),
                        _ => panic!(),
                    },
                    UpdateOp::SuffixSub => match old {
                        Value::Int(i) => (Value::Int(i), Value::Int(i - 1)),
                        Value::Number(n) => (Value::Number(n), Value::Number(n - 1.0)),
                        Value::Bigint(n) => (Value::Bigint(n), Value::Bigint(n - 1)),
                        _ => panic!(),
                    },
                };

                self.context.write(*variable, nv);

                return Ok(rv);
            }
            Expr::Bin { op, left, right } => {
                let a = self.run_expr(&left)?;
                let b = self.run_expr(&right)?;

                let v = match op {
                    BinOp::Add => self.add(a, b),
                    BinOp::Sub => self.sub(a, b),
                    BinOp::Mul => self.mul(a, b),
                    BinOp::Div => self.div(a, b),
                    BinOp::Mod => self.rem(a, b),
                    BinOp::BitAnd => self.bitand(a, b),
                    BinOp::BitOr => self.bitor(a, b),
                    BinOp::BitXor => self.bixor(a, b),
                    BinOp::RShift => self.rshift(a, b),
                    BinOp::LShift => self.lshift(a, b),
                    BinOp::URShift => self.zero_fill_rshift(a, b),
                    BinOp::Exp => self.exp(a, b)?,
                    BinOp::EqEq => Value::Bool(self.equals(&a, &b)),
                    BinOp::EqEqEq => Value::Bool(self.strict_equal(&a, &b)),
                    BinOp::NotEq => Value::Bool(!self.equals(&a, &b)),
                    BinOp::NotEqEq => Value::Bool(!self.strict_equal(&a, &b)),
                    BinOp::And => self.and(a, b),
                    BinOp::Or => self.or(a, b),
                    BinOp::Gt => self.gt(a, b),
                    BinOp::Gteq => self.gteq(a, b),
                    BinOp::Lt => self.lt(a, b),
                    BinOp::Lteq => self.lteq(a, b),
                    BinOp::Nullish => self.nullish(a, b),
                    BinOp::In => Value::Bool(self.get_property_expr(&a, &b)?.is_some()),
                };

                return Ok(v);
            }
            Expr::Unary { op, value } => {
                let value = self.run_expr(&value)?;

                let v = match op {
                    UnaryOp::Void => Value::Undefined,
                    UnaryOp::LogicalNot => Value::Bool(!self.to_bool(&value)),
                    UnaryOp::BitNot => match value {
                        Value::Int(i) => Value::Int(!i),
                        Value::Number(n) => Value::Int(!(n as i32)),
                        Value::Bigint(i) => Value::Bigint(!i),
                        _ => panic!(),
                    },
                    UnaryOp::Plus => match value {
                        Value::Int(i) => Value::Int(i),
                        Value::Number(n) => Value::Number(n),
                        Value::Bigint(i) => Value::Bigint(i),
                        _ => panic!(),
                    },
                    UnaryOp::Minus => match value {
                        Value::Int(i) => Value::Int(-i),
                        Value::Number(n) => Value::Number(-n),
                        Value::Bigint(i) => Value::Bigint(-i),
                        _ => panic!(),
                    },
                    UnaryOp::Typeof => {
                        let s = match value {
                            Value::Int(_) | Value::Number(_) => "number",
                            Value::Bigint(_) => "bigint",
                            Value::Null => "null",
                            Value::Undefined => "undefined",
                            Value::Bool(_) => "boolean",
                            Value::String(_) => "string",
                            Value::Symbol(_) => "symbol",
                            _ => "object",
                        };
                        Value::String(s.to_string())
                    }
                };

                return Ok(v);
            }
            Expr::Ternary { test, left, right } => {
                let test = self.run_expr(&test)?;
                let left = self.run_expr(&left)?;
                let right = self.run_expr(&right)?;

                if self.to_bool(&test) {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            Expr::Seq(a, b) => {
                let _ = self.run_expr(&a)?;
                let b = self.run_expr(&b)?;

                return Ok(b);
            }
            Expr::Await(p) => {
                let p = self.run_expr(&p)?;
                todo!()
            }
            Expr::Yield(y) => {
                let y = self.run_expr(&y)?;
                todo!()
            }
            Expr::Closure(c) => {
                let func = self.table.functions.get(c).expect("invalid function");

                let mut cap = HashMap::new();

                for (vid, _) in &func.captures {
                    let v = self
                        .context
                        .heap_variables
                        .get(vid)
                        .expect("invalid variable");
                    cap.insert(*vid, v.clone());
                }

                return Ok(Value::Clousure {
                    id: *c,
                    captures: Arc::new(cap),
                });
            }
            Expr::Cast(v, ty) => {
                let v = self.run_expr(&v)?;
                return Ok(self.cast(&v, ty));
            }
            Expr::AssertNonNull(v) => {
                let v = self.run_expr(&v)?;

                return Ok(self.assert_not_null(v)?);
            }
        }
    }

    fn cast(&mut self, value: &Value, ty: &Type) -> Value {
        match value {
            Value::Any(value) => self.cast(value, ty),
            Value::AnyObject(value) => self.cast(value, ty),
            Value::Interface { value, .. } => self.cast(value, ty),
            Value::Union { value, .. } => self.cast(value, ty),
            Value::Bigint(b) => match ty {
                Type::Bigint => value.clone(),
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::Bool => Value::Bool(*b != 0),
                Type::Int => Value::Int(*b as i32),
                Type::Number => Value::Number(*b as f64),
                Type::String => Value::String(b.to_string()),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            Value::Int(i) => match ty {
                Type::Int => value.clone(),
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::Bool => Value::Bool(*i != 0),
                Type::Number => Value::Number(*i as f64),
                Type::Bigint => Value::Bigint(*i as i128),
                Type::String => Value::String(i.to_string()),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            Value::Number(f) => match ty {
                Type::Number => value.clone(),
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::Bool => Value::Bool(!f.is_nan() && *f != 0.0),
                Type::Int => Value::Int(*f as i32),
                Type::Bigint => Value::Bigint(*f as i128),
                Type::String => Value::String(f.to_string()),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            Value::Bool(b) => match ty {
                Type::Bool => Value::Bool(*b),
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::Int => Value::Int(*b as i32),
                Type::Number => Value::Number(if *b { 1.0 } else { 0.0 }),
                Type::Bigint => Value::Bigint(*b as i128),
                Type::String => Value::String(b.to_string()),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            Value::Tuple(t) => match ty {
                Type::Array(_) => Value::Array(t.clone()),
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::AnyObject => Value::AnyObject(Box::new(Value::Tuple(t.clone()))),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            Value::Object { id, values } => match ty {
                Type::Object(super_class) => {
                    todo!()
                }
                Type::LiteralObject(o) => {
                    todo!()
                }
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::AnyObject => Value::AnyObject(Box::new(value.clone())),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
            _ => match ty {
                Type::Any => Value::Any(Box::new(value.clone())),
                Type::AnyObject => Value::AnyObject(Box::new(value.clone())),
                Type::Interface(id) => Value::Interface {
                    id: *id,
                    value: Box::new(value.clone()),
                },
                Type::Union(u) => Value::Union {
                    ty: u.clone(),
                    value: Box::new(value.clone()),
                },
                _ => panic!("invalid cast"),
            },
        }
    }

    fn get_property(&self, value: &Value, prop: &PropName) -> Option<Value> {
        match value {
            Value::Array(a) => match prop {
                PropName::Ident(s) => match s.as_str() {
                    "length" => Some(Value::Int(a.read().len() as i32)),
                    _ => None,
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn get_property_expr(&self, value: &Value, key: &Value) -> Result<Option<Value>, Value> {
        match value {
            Value::Any(value) => self.get_property_expr(value, key),
            Value::AnyObject(value) => self.get_property_expr(value, key),
            Value::Interface { value, .. } => self.get_property_expr(value, key),
            Value::Union { value, .. } => self.get_property_expr(value, key),
            Value::Array(a) => match key {
                Value::Int(i) => {
                    if let Some(v) = a.read().get(*i as usize) {
                        Ok(Some(v.clone()))
                    } else {
                        Err(Value::String(format!("index out of range: {}", *i)))
                    }
                }
                Value::Number(i) => {
                    if let Some(v) = a.read().get(*i as usize) {
                        Ok(Some(v.clone()))
                    } else {
                        Err(Value::String(format!("index out of range: {}", *i)))
                    }
                }
                _ => Ok(None),
            },
            Value::Tuple(a) => match key {
                Value::Int(i) => {
                    if let Some(v) = a.read().get(*i as usize) {
                        Ok(Some(v.clone()))
                    } else {
                        Err(Value::String(format!("index out of range: {}", *i)))
                    }
                }
                Value::Number(i) => {
                    if let Some(v) = a.read().get(*i as usize) {
                        Ok(Some(v.clone()))
                    } else {
                        Err(Value::String(format!("index out of range: {}", *i)))
                    }
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn set_property(&self, obj: &Value, prop: &PropName, value: Value) {
        todo!()
    }

    fn set_property_expr(&self, obj: &Value, key: Value, value: Value) {
        todo!()
    }

    fn strict_equal(&self, left: &Value, right: &Value) -> bool {
        match right {
            Value::Any(v) => return self.strict_equal(left, v),
            Value::AnyObject(v) => return self.strict_equal(left, &v),
            Value::Interface { value, .. } => return self.strict_equal(left, &value),
            Value::Union { value, .. } => return self.strict_equal(left, &value),
            _ => {}
        }
        match left {
            Value::Any(v) => return self.strict_equal(v, right),
            Value::AnyObject(v) => return self.strict_equal(&v, right),
            Value::Interface { value, .. } => return self.strict_equal(&value, right),
            Value::Union { value, .. } => return self.strict_equal(&value, right),
            Value::Bigint(i) => match right {
                Value::Bigint(n) => i == n,
                _ => false,
            },
            Value::Bool(a) => match right {
                Value::Bool(b) => a == b,
                _ => false,
            },
            Value::Clousure {
                id: id1,
                captures: cap1,
            } => match right {
                Value::Clousure {
                    id: id2,
                    captures: cap2,
                } => id1 == id2 && Arc::as_ptr(cap1) == Arc::as_ptr(cap2),
                _ => false,
            },

            Value::Enum {
                id: id1,
                variant: var1,
            } => match right {
                Value::Enum {
                    id: id2,
                    variant: var2,
                } => *id1 == *id2 && *var1 == *var2,
                _ => false,
            },
            Value::Function(f1) => match right {
                Value::Function(f2) => *f1 == *f2,
                _ => false,
            },
            Value::Int(i) => match right {
                Value::Int(n) => i == n,
                Value::Number(n) => *i as f64 == *n,
                _ => false,
            },
            Value::Number(i) => match right {
                Value::Int(n) => (*n as f64) == *i,
                Value::Number(n) => i == n,
                _ => false,
            },
            Value::Null => match right {
                Value::Null => true,
                _ => false,
            },
            Value::DynObject { values: o1 } => match right {
                Value::DynObject { values: o2 } => Arc::as_ptr(o1) == Arc::as_ptr(o2),
                Value::Object { values: o2, .. } => Arc::as_ptr(o1) == Arc::as_ptr(o2),
                _ => false,
            },
            Value::Object { values: o1, .. } => match right {
                Value::Object { values: o2, .. } => Arc::as_ptr(o1) == Arc::as_ptr(o2),
                Value::DynObject { values: o2 } => Arc::as_ptr(o1) == Arc::as_ptr(o2),
                _ => false,
            },
            Value::Regex() => match right {
                Value::Regex() => true,
                _ => false,
            },
            Value::String(s1) => match right {
                Value::String(s2) => s1 == s2,
                _ => false,
            },
            Value::Symbol(s1) => match right {
                Value::Symbol(s2) => s1 == s2,
                _ => false,
            },
            Value::Undefined => match right {
                Value::Undefined => true,
                _ => false,
            },
            Value::Array(a) => match right {
                Value::Array(b) => Arc::as_ptr(a) == Arc::as_ptr(b),
                Value::Tuple(b) => Arc::as_ptr(a) == Arc::as_ptr(b),
                _ => false,
            },
            Value::Tuple(a) => match right {
                Value::Array(b) => Arc::as_ptr(a) == Arc::as_ptr(b),
                Value::Tuple(b) => Arc::as_ptr(a) == Arc::as_ptr(b),
                _ => false,
            },
        }
    }

    fn equals(&self, a: &Value, b: &Value) -> bool {
        println!("{:?} == {:?}", a, b);
        self.strict_equal(a, b)
    }

    fn get_type(&self, value: &Value) -> Type {
        todo!()
    }

    fn assert_type(&self, value: &Value, ty: &Type) {
        //assert!(&self.get_type(value) == ty)
    }

    fn is_nullable(&self, value: &Value) -> bool {
        match value {
            Value::Null => true,
            Value::Undefined => true,
            Value::Any(v) => self.is_nullable(v),
            Value::Interface { value, .. } => self.is_nullable(&value),
            Value::Union { value, .. } => self.is_nullable(&value),
            _ => false,
        }
    }

    fn assert_not_null(&self, value: Value) -> Result<Value, Value> {
        match value {
            Value::Null | Value::Undefined => {
                Err(Value::String("assert not null failed".to_string()))
            }
            Value::Any(a) => self.assert_not_null(*a),
            Value::Interface { value, .. } => self.assert_not_null(*value),
            Value::Union { ty, value } => {
                if self.is_nullable(&value) {
                    return Err(Value::String("assert not null failed".to_string()));
                }

                let mut tys = ty.to_vec();
                tys.retain(|t| t != &Type::Null && t != &Type::Undefined);

                if tys.len() == 1 {
                    return Ok(*value);
                }
                return Ok(Value::Union {
                    ty: tys.into(),
                    value: value,
                });
            }
            _ => Ok(value),
        }
    }

    fn add(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Int(i + n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Number(i + n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i + n),
                _ => panic!(),
            },
            Value::String(i) => match b {
                Value::String(n) => Value::String(i + n.as_str()),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn sub(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Int(i - n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Number(i - n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i - n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn mul(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Int(i * n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Number(i * n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i * n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    fn div(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Number((i as f64) / (n as f64)),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Number(i / n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i / n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn exp(&self, a: Value, b: Value) -> Result<Value, Value> {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => {
                    if n < 0 {
                        Ok(Value::Number((i as f64).powi(n)))
                    } else {
                        Ok(Value::Int(i.pow(n as u32)))
                    }
                }
                //Value::Number(n) => {
                //    Ok(Value::Number((i as f64).powf(n)))
                //}
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Ok(Value::Number(i.powf(n))),
                //Value::Int(n) => Ok(Value::Number(i.powi(n))),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => {
                    if n < 0 || n > u32::MAX as _ {
                        Err(Value::String(
                            "RangeError: Exponent must be positive".to_string(),
                        ))
                    } else {
                        if let Some(v) = i.checked_pow(i as u32) {
                            Ok(Value::Bigint(v))
                        } else {
                            Err(Value::String(
                                "RangeError: Exponent must be positive".to_string(),
                            ))
                        }
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn rem(&self, a: Value, b: Value) -> Value {
        println!("{:?} % {:?}", a, b);
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i % b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Number(i % n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i % n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn lshift(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i << b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int((i as i32) << (n as i32)),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i << n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn rshift(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i >> b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int((i as i32) >> (n as i32)),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i >> n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn zero_fill_rshift(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(((i as u32) >> (b as u32)) as i32),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int(((i as i32 as u32) >> (n as i32 as u32)) as i32),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint((i as u128 >> n as u128) as i128),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn bitand(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i & b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int((i as i32) & (n as i32)),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i & n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn bitor(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i | b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int((i as i32) | (n as i32)),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i | n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn bixor(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(b) => Value::Int(i ^ b),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Int((i as i32) ^ (n as i32)),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bigint(i ^ n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn gt(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Bool(i > n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Bool(i > n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bool(i > n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn gteq(&self, a: Value, b: Value) -> Value {
        println!("{:?} >= {:?}", a, b);
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Bool(i >= n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Bool(i >= n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bool(i >= n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn lt(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Bool(i < n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Bool(i < n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bool(i < n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn lteq(&self, a: Value, b: Value) -> Value {
        match a {
            Value::Int(i) => match b {
                Value::Int(n) => Value::Bool(i <= n),
                _ => panic!(),
            },
            Value::Number(i) => match b {
                Value::Number(n) => Value::Bool(i <= n),
                _ => panic!(),
            },
            Value::Bigint(i) => match b {
                Value::Bigint(n) => Value::Bool(i <= n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn to_bool(&self, value: &Value) -> bool {
        match value {
            Value::Any(v) => self.to_bool(v),
            Value::AnyObject(v) => self.to_bool(v),
            Value::Array(_) => true,
            Value::Bigint(i) => *i != 0,
            Value::Bool(b) => *b,
            Value::Number(i) => !i.is_nan() && *i != 0.0,
            Value::Int(i) => *i != 0,
            Value::Function(_) => true,
            Value::Clousure { .. } => true,
            Value::Object { .. } => true,
            Value::DynObject { .. } => true,
            Value::Regex() => true,
            Value::Symbol(_) => true,
            Value::String(s) => !s.is_empty(),
            Value::Interface { value, .. } => self.to_bool(&value),
            Value::Tuple(_) => true,
            Value::Union { value, .. } => self.to_bool(&value),
            Value::Null => false,
            Value::Undefined => false,
            Value::Enum { .. } => true,
        }
    }

    fn and(&self, a: Value, b: Value) -> Value {
        return Value::Bool(self.to_bool(&a) && self.to_bool(&b));
    }

    fn or(&self, a: Value, b: Value) -> Value {
        if self.to_bool(&a) {
            return a;
        } else {
            return b;
        }
    }

    fn nullish(&self, a: Value, b: Value) -> Value {
        if self.is_nullable(&a) {
            return b;
        } else {
            return a;
        }
    }
}
