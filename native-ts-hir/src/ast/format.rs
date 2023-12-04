use crate::{
    common::{FunctionId, VariableId},
    PropName,
};

use super::{Callee, Expr, Module, PropNameOrExpr, Stmt, Type, UnaryOp, UpdateOp};
use crate::symbol_table::SymbolTable;

pub struct Formatter<'a> {
    spaces: usize,
    buf: String,
    table: &'a SymbolTable,
}

impl<'a> Formatter<'a> {
    pub const fn new(table: &'a SymbolTable) -> Self {
        Self {
            spaces: 0,
            buf: String::new(),
            table,
        }
    }
    pub fn emit_string(&mut self) -> String {
        core::mem::replace(&mut self.buf, String::new())
    }
    fn emit_spaces(&mut self) {
        for _ in 0..self.spaces {
            self.buf.push(' ')
        }
    }
    fn new_scope(&mut self) {
        self.spaces += 4;
    }
    fn close_scope(&mut self) {
        self.spaces -= 4;
    }
    fn write_str(&mut self, s: &str) {
        self.buf.push_str(s);
    }
    fn write_int<I: itoa::Integer>(&mut self, i: I) {
        let mut buf = itoa::Buffer::new();
        self.write_str(buf.format(i))
    }
    pub fn format_module(&mut self, m: &Module) {
        let func = self
            .table
            .functions
            .get(&m.main_function)
            .expect("invalid function");
        for stmt in &func.stmts {
            self.format_stmt(stmt);
        }
    }
    pub fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block { label } => {
                self.emit_spaces();
                self.write_str(&label);
                self.write_str(":{\n");

                self.new_scope()
            }
            Stmt::EndBlock => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::DeclareClass(id) => {
                let class = self.table.classes.get(id).expect("invalid class");

                self.write_str("class class");
                self.write_int(id.0);

                if let Some(sup) = class.extends {
                    self.write_str(" extends class");
                    self.write_int(sup.0);
                };

                if !class.implements.is_empty() {
                    self.write_str(" implements ");
                    for (i, iface) in class.implements.iter().enumerate() {
                        self.write_str("iface");
                        self.write_int(iface.0);

                        if i + 1 != class.implements.len() {
                            self.write_str(", ")
                        }
                    }
                }

                self.write_str("{\n");
                self.new_scope();

                if let Some((id, _)) = &class.constructor {
                    self.emit_spaces();
                    self.write_str("constructor");
                    self.format_function_body(*id);
                }

                for (name, (_, ty)) in &class.static_properties {
                    self.emit_spaces();
                    self.write_str("static ");
                    self.format_propname(name);
                    self.write_str(":");
                    self.format_ty(ty);
                    self.write_str(";\n")
                }

                for (name, prop) in &class.properties {
                    self.emit_spaces();
                    if prop.readonly {
                        self.write_str("readonly ");
                    }
                    self.format_propname(name);
                    self.write_str(":");
                    self.format_ty(&prop.ty);

                    if let Some(init) = &prop.initialiser {
                        self.write_str("=");
                        self.format_expr(init);
                        self.write_str(";\n");
                    }
                }

                for (name, (id, _)) in &class.static_methods {
                    self.emit_spaces();
                    self.write_str("static function ");
                    self.format_propname(name);
                    self.format_function_body(*id);
                }

                for (name, (id, _)) in &class.methods {
                    self.emit_spaces();
                    self.write_str("function ");
                    self.format_propname(name);
                    self.format_function_body(*id);
                }

                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::DeclareFunction(id) => {
                self.emit_spaces();
                self.write_str("function fun");
                self.write_int(id.0);
                self.format_function_body(*id);
            }
            Stmt::DeclareGenericClass(_id) => {}
            Stmt::DeclareGenericFunction(_id) => {}
            Stmt::DeclareGenericInterface(_id) => {}
            Stmt::DeclareInterface(id) => {
                let iface = self.table.interfaces.get(id).expect("invalid interface");
            }
            Stmt::DeclareVar(id, ty) => {
                self.emit_spaces();
                self.write_str("var var");
                self.write_int(id.0);
                self.write_str(":");
                self.format_ty(ty);
                self.write_str("\n");
            }
            Stmt::DropVar(_) => {}
            Stmt::If { test } => {
                self.emit_spaces();
                self.write_str("if (");
                self.format_expr(test);
                self.write_str("){\n");

                self.new_scope();
            }
            Stmt::EndIf => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n")
            }
            Stmt::Else => {
                self.emit_spaces();
                self.write_str("else {\n");
                self.new_scope();
            }
            Stmt::EndElse => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n")
            }
            Stmt::Switch(test) => {
                self.emit_spaces();
                self.write_str("switch (");
                self.format_expr(test);
                self.write_str("){\n");

                self.new_scope();
            }
            Stmt::EndSwitch => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::SwitchCase(test) => {
                self.emit_spaces();
                self.write_str("case ");
                self.format_expr(test);
                self.write_str(":\n");
                self.new_scope();
            }
            Stmt::EndSwitchCase => {
                self.emit_spaces();
                self.write_str("break;\n");
                self.close_scope();
            }
            Stmt::DefaultCase => {
                self.emit_spaces();
                self.write_str("default:\n");
                self.new_scope();
            }
            Stmt::EndDefaultCase => {
                self.emit_spaces();
                self.write_str("break;");
                self.close_scope();
            }
            Stmt::Loop { label } => {
                self.emit_spaces();

                if let Some(label) = label {
                    self.write_str(&label);
                    self.write_str(":");
                }

                self.write_str("for (;;){\n");
                self.new_scope();
            }
            Stmt::EndLoop => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}");
            }
            Stmt::Try => {
                self.emit_spaces();
                self.write_str("try {\n");
                self.new_scope();
            }
            Stmt::EndTry => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::Catch(id, ty) => {
                self.emit_spaces();
                self.write_str("catch (");
                self.write_var(*id);
                self.write_str(":");
                self.format_ty(ty);
                self.write_str("){\n");
                self.new_scope();
            }
            Stmt::EndCatch => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::Finally => {
                self.emit_spaces();
                self.write_str("finally {\n");
                self.new_scope();
            }
            Stmt::EndFinally => {
                self.close_scope();
                self.emit_spaces();
                self.write_str("}\n");
            }
            Stmt::Break(label) => {
                self.emit_spaces();

                if let Some(label) = label {
                    self.write_str("break ");
                    self.write_str(&label);
                    self.write_str("\n")
                } else {
                    self.write_str("break\n");
                }
            }
            Stmt::Continue(label) => {
                self.emit_spaces();

                if let Some(label) = label {
                    self.write_str("continue ");
                    self.write_str(&label);
                    self.write_str("\n")
                } else {
                    self.write_str("continue\n");
                }
            }
            Stmt::Return(r) => {
                self.emit_spaces();
                self.write_str("return ");
                self.format_expr(r);
                self.write_str("\n");
            }
            Stmt::Throw(t) => {
                self.emit_spaces();
                self.write_str("throw ");
                self.format_expr(t);
                self.write_str("\n")
            }
            Stmt::Expr(e) => {
                self.emit_spaces();
                self.format_expr(e);
                self.write_str("\n")
            }
        }
    }

    fn write_var(&mut self, id: VariableId) {
        self.write_str("var");
        let mut buf = native_js_common::itoa::Buffer::new();
        self.write_str(buf.format(id.0));
    }

    pub fn format_ty(&mut self, ty: &Type) {
        match ty {
            Type::Alias(_) => unreachable!(),
            Type::Any => self.write_str("any"),
            Type::AnyObject => self.write_str("object"),
            Type::Array(a) => {
                self.format_ty(a);
                self.write_str("[]")
            }
            Type::Bigint => self.write_str("bigint"),
            Type::Bool => self.write_str("boolean"),
            Type::Enum(e) => {
                self.write_str("enum");
                self.write_int(e.0);
            }
            Type::Function(f) => {
                self.write_str("(this:");
                self.format_ty(&f.this_ty);

                for p in &f.params {
                    self.write_str(",");
                    self.format_ty(p);
                }
                self.write_str(")=>");
                self.format_ty(&f.return_ty);
            }
            Type::Interface(id) => {
                self.write_str("iface");
                self.write_int(id.0);
            }
            Type::Object(id) => {
                self.write_str("class");
                self.write_int(id.0);
            }
            Type::Generic(id) => {
                self.write_str("generic");
                self.write_int(id.0);
            }
            Type::Int | Type::Number => self.write_str("number"),
            Type::Map(k, v) => {
                self.write_str("Map<");
                self.format_ty(k);
                self.write_str(",");
                self.format_ty(v);
                self.write_str(">");
            }
            Type::Iterator(e) => {
                self.write_str("Iterator<");
                self.format_ty(e);
                self.write_str(">")
            }
            Type::Null => self.write_str("null"),
            Type::Promise(p) => {
                self.write_str("Promise<");
                self.format_ty(p);
                self.write_str(">");
            }
            Type::Regex => self.write_str("Regex"),
            Type::String => self.write_str("string"),
            Type::Symbol => self.write_str("symbol"),
            Type::Undefined => self.write_str("undefined"),
            Type::Tuple(tu) => {
                self.write_str("[");
                for (i, t) in tu.iter().enumerate() {
                    self.format_ty(t);

                    if i != tu.len() - 1 {
                        self.write_str(",")
                    }
                }
                self.write_str("]")
            }
            Type::Union(u) => {
                for (i, t) in u.iter().enumerate() {
                    self.format_ty(t);
                    if i != u.len() - 1 {
                        self.write_str(" | ")
                    }
                }
            }
        }
    }
    pub fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Array { values } => {
                self.write_str("[");

                for (i, v) in values.iter().enumerate() {
                    self.format_expr(v);
                    if i != values.len() - 1 {
                        self.write_str(",")
                    }
                }
                self.write_str("]")
            }
            Expr::AssertNonNull(e) => {
                self.format_expr(e);
                self.write_str("!");
            }
            Expr::Await(a) => {
                self.write_str("await ");
                self.format_expr(a);
            }
            Expr::Bigint(i) => {
                let mut buf = itoa::Buffer::new();
                self.write_str(buf.format(*i));
                self.write_str("n")
            }
            Expr::Bin { op, left, right } => {
                self.write_str("(");
                self.format_expr(&left);
                self.write_str(")");

                self.write_str(op.as_str());

                self.write_str("(");
                self.format_expr(&right);
                self.write_str(")");
            }
            Expr::Bool(b) => {
                if *b {
                    self.write_str("true")
                } else {
                    self.write_str("false")
                }
            }
            Expr::Call {
                callee,
                args,
                optional,
            } => {
                match callee.as_ref() {
                    Callee::Expr(e) => self.format_expr(e),
                    Callee::Function(id) => {
                        self.write_str("fun");
                        self.write_int(id.0)
                    }
                    Callee::Member { object, prop } => {
                        self.format_expr(object);
                        self.format_propname_or_expr(prop);
                    }
                    Callee::Super(_) => self.write_str("super"),
                };

                if *optional {
                    self.write_str("?.(")
                } else {
                    self.write_str("(");
                }

                for (i, arg) in args.iter().enumerate() {
                    self.format_expr(arg);

                    if i != args.len() - 1 {
                        self.write_str(",")
                    }
                }

                self.write_str(")")
            }
            Expr::Cast(e, ty) => {
                self.format_expr(e);
                self.write_str(" as ");
                self.format_ty(ty);
            }
            Expr::Closure(id) => {
                self.write_str("function fun");
                self.write_int(id.0);
                self.format_function_body(*id);
            }
            Expr::Function(id) => {
                self.write_str("fun");
                self.write_int(id.0);
            }
            Expr::Int(i) => self.write_int(*i),
            Expr::Number(f) => self.write_str(&f.to_string()),
            Expr::Member {
                object,
                key,
                optional,
            } => {
                self.format_expr(&object);

                if *optional {
                    if let PropNameOrExpr::PropName(PropName::Ident(id)) = key {
                        self.write_str("?.");
                        self.write_str(&id);
                    } else {
                        self.write_str("?.");
                        self.format_propname_or_expr(key);
                    }
                } else {
                    self.format_propname_or_expr(key);
                }
            }
            Expr::MemberAssign {
                op,
                object,
                key,
                value,
            } => {
                self.format_expr(&object);
                self.format_propname_or_expr(key);

                self.write_str(op.as_str());
                self.format_expr(&value);
            }
            Expr::MemberUpdate { op, object, key } => {
                match op {
                    UpdateOp::PrefixAdd => self.write_str("++"),
                    UpdateOp::PrefixSub => self.write_str("--"),
                    _ => {}
                }
                self.format_expr(&object);
                self.format_propname_or_expr(key);

                match op {
                    UpdateOp::SuffixAdd => self.write_str("++"),
                    UpdateOp::SuffixSub => self.write_str("--"),
                    _ => {}
                }
            }
            Expr::New { class, args } => {
                self.write_str("new class");
                self.write_int(class.0);

                self.write_str("(");

                for (i, arg) in args.iter().enumerate() {
                    self.format_expr(arg);
                    if i != args.len() - 1 {
                        self.write_str(",")
                    }
                }
                self.write_str(")");
            }
            Expr::Null => self.write_str("null"),
            Expr::Regex() => {}
            Expr::Seq(a, b) => {
                self.write_str("(");
                self.format_expr(a);
                self.write_str(",");
                self.format_expr(b);
                self.write_str(")")
            }
            Expr::String(s) => {
                self.write_str("\"");
                self.write_str(s);
                self.write_str("\"");
            }
            Expr::Symbol(s) => {
                self.write_str(&s.to_string());
            }
            Expr::Ternary { test, left, right } => {
                self.format_expr(&test);
                self.write_str("?");
                self.format_expr(&left);
                self.write_str(":");
                self.format_expr(&right);
            }
            Expr::This => self.write_str("this"),
            Expr::Tuple { values } => {
                self.write_str("[");

                for (i, v) in values.iter().enumerate() {
                    self.format_expr(v);
                    if i != values.len() - 1 {
                        self.write_str(",")
                    }
                }
                self.write_str("]")
            }
            Expr::Unary { op, value } => {
                let op = match op {
                    UnaryOp::BitNot => "~",
                    UnaryOp::LogicalNot => "!",
                    UnaryOp::Minus => "-",
                    UnaryOp::Plus => "+",
                    UnaryOp::Typeof => "typeof ",
                    UnaryOp::Void => "void ",
                };
                self.write_str(op);
                self.write_str("(");
                self.format_expr(&value);
                self.write_str(")");
            }
            Expr::Undefined => self.write_str("undefined"),
            Expr::VarAssign {
                op,
                variable,
                value,
            } => {
                self.write_str("var");
                self.write_int(variable.0);

                self.write_str(op.as_str());

                self.format_expr(&value);
            }
            Expr::VarLoad { span: _, variable } => {
                self.write_str("var");
                self.write_int(variable.0);
            }
            Expr::VarUpdate { op, variable } => {
                match op {
                    UpdateOp::PrefixAdd => self.write_str("++"),
                    UpdateOp::PrefixSub => self.write_str("--"),
                    _ => {}
                }
                self.write_str("var");
                self.write_int(variable.0);

                match op {
                    UpdateOp::SuffixAdd => self.write_str("++"),
                    UpdateOp::SuffixSub => self.write_str("--"),
                    _ => {}
                }
            }
            Expr::Yield(y) => {
                self.write_str("yield ");
                self.format_expr(y);
            }
        }
    }

    fn format_function_body(&mut self, id: FunctionId) {
        let func = self.table.functions.get(&id).expect("invalid function");
        self.write_str("(this:");
        self.format_ty(&func.this_ty);

        for p in func.params.iter() {
            self.write_str(", var");
            self.write_int(p.id.0);
            self.write_str(":");
            self.format_ty(&p.ty);
        }
        self.write_str("):");
        self.format_ty(&func.return_ty);
        self.write_str("{\n");
        self.new_scope();

        for s in &func.stmts {
            self.format_stmt(s);
        }

        self.close_scope();
        self.emit_spaces();
        self.write_str("}\n");
    }

    fn format_propname_or_expr(&mut self, prop: &PropNameOrExpr) {
        match prop {
            PropNameOrExpr::PropName(p) => self.format_propname(p),
            PropNameOrExpr::Expr(e, _ty) => {
                self.write_str("[");
                self.format_expr(e);
                self.write_str("]")
            }
        }
    }

    fn format_propname(&mut self, prop: &PropName) {
        match prop {
            PropName::Ident(id) => {
                self.write_str(".");
                self.write_str(id);
            }
            PropName::Int(i) => {
                self.write_str("[");
                self.write_int(*i);
                self.write_str("]");
            }
            PropName::Private(p) => {
                self.write_str(".#");
                self.write_str(p);
            }
            PropName::String(s) => {
                self.write_str("[\"");
                self.write_str(s);
                self.write_str("\"]");
            }
            PropName::Symbol(s) => {
                self.write_str("[");
                self.write_str(&s.to_string());
                self.write_str("]")
            }
        }
    }
}
