use std::collections::{HashMap, HashSet};

use swc_atoms::JsWord;

use super::class::ClassID;
use super::context::*;
use super::{ir::IR, FunctionId, IRContainer, VariableId};

pub struct IRModule {
    //pub id: ModuleId,
    //pub exports: HashMap<JsWord, ExportValue>,
    //pub default_export: Option<ExportValue>,
    //pub variables: Vec<(VariableId, Type)>,
    //pub functions: HashMap<FunctionId, IRFunction>,
    pub heap_variables: Vec<VariableId>,

    pub own_variable: HashSet<VariableId>,
    var_declares: HashMap<JsWord, VariableId>,
    bindings: Context<JsWord, VariableId>,
    context: Context<JsWord, Variable>,

    pub ir: Vec<IR>,
}

impl IRModule {
    pub fn new() -> Self {
        Self {
            own_variable: HashSet::new(),
            heap_variables: Default::default(),
            var_declares: HashMap::new(),
            bindings: Context::new(),
            context: Context::new(),
            ir: Vec::new(),
        }
    }
}

impl IRContainer for IRModule {
    fn _add_heap_variable(&mut self, varid: VariableId) {
        self.heap_variables.push(varid);
    }

    fn new_context(&mut self) {
        self.bindings.new_layer();
        self.context.new_layer();
    }

    fn close_context(&mut self) {
        self.bindings.remove_layer();
        self.context.remove_layer();
    }

    fn is_function(&self) -> bool {
        false
    }

    fn is_class_function(&self) -> Option<ClassID> {
        None
    }

    fn is_constructor(&self) -> bool {
        false
    }

    fn is_async(&self) -> bool {
        true
    }
    fn is_generator(&self) -> bool {
        false
    }

    fn is_global(&self) -> bool {
        true
    }

    fn push(&mut self, ir: IR) {
        self.ir.push(ir);
    }

    fn owns_variable(&self, varid: VariableId) -> bool {
        self.own_variable.contains(&varid)
    }

    fn bind_variable(&mut self, name: &JsWord) -> VariableId {
        let varid = VariableId::new();
        self.bindings.set(name.clone(), varid);

        self.own_variable.insert(varid);
        self.push(IR::DeclareVar(varid));
        return varid;
    }

    fn read_var(&mut self, name: &JsWord) -> Option<Variable> {
        if let Some(v) = self.context.get(name) {
            return Some(*v);
        }

        if let Some(v) = self.var_declares.get(name) {
            return Some(Variable::Var(*v));
        }

        self.bindings.get(name).map(|v| Variable::Binding(*v))
    }

    fn declare_anonymos(&mut self) -> VariableId {
        let id = VariableId::new();
        self.push(IR::DeclareVar(id));
        self.own_variable.insert(id);
        return id;
    }

    fn declare_var(&mut self, name: &JsWord) -> Result<VariableId, String> {
        if self.context.is_top_layer() && self.context.has_name_on_surface(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        if let Some(varid) = self.var_declares.get(name) {
            return Ok(*varid);
        } else {
            let varid = VariableId::new();
            self.var_declares.insert(name.clone(), varid);
            self.own_variable.insert(varid);
            self.push(IR::DeclareVar(varid));
            return Ok(varid);
        }
    }

    fn declare_class(&mut self, name: &JsWord, classid: ClassID) -> Result<VariableId, String> {
        if self.context.has_name_on_surface(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        if self.context.is_top_layer() && self.var_declares.contains_key(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        let id = VariableId::new();
        self.context.set(name.clone(), Variable::Class(id, classid));
        self.own_variable.insert(id);
        self.push(IR::DeclareVar(id));

        return Ok(id);
    }

    fn declare_function(
        &mut self,
        name: &JsWord,
        funcid: FunctionId,
    ) -> Result<VariableId, String> {
        if self.context.has_name_on_surface(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        if self.context.is_top_layer() && self.var_declares.contains_key(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        let id = VariableId::new();
        self.context
            .set(name.clone(), Variable::Function(id, funcid));

        self.own_variable.insert(id);
        self.push(IR::DeclareVar(id));

        return Ok(id);
    }

    fn declare_const(&mut self, name: &JsWord) -> Result<VariableId, String> {
        if self.context.has_name_on_surface(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        if self.context.is_top_layer() && self.var_declares.contains_key(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        let id = VariableId::new();
        self.context.set(name.clone(), Variable::Const(id));
        self.own_variable.insert(id);

        self.push(IR::DeclareVar(id));

        return Ok(id);
    }

    fn declare_let(&mut self, name: &JsWord) -> Result<VariableId, String> {
        if self.context.has_name_on_surface(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        if self.context.is_top_layer() && self.var_declares.contains_key(name) {
            return Err(format!("Duplicate identifier '{}'.", name));
        }

        let id = VariableId::new();
        self.context.set(name.clone(), Variable::Let(id));
        self.own_variable.insert(id);

        self.push(IR::DeclareVar(id));

        return Ok(id);
    }

    fn get_class(&self, name: &JsWord) -> (ClassID, VariableId) {
        if let Variable::Class(id, classid) = self.context.get(name).unwrap() {
            return (*classid, *id);
        } else {
            panic!()
        }
    }

    fn get_function(&self, name: &JsWord) -> (FunctionId, VariableId) {
        if let Variable::Function(id, funcid) = self.context.get(name).unwrap() {
            return (*funcid, *id);
        } else {
            panic!()
        }
    }
}
