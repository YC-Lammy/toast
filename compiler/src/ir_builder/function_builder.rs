use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use swc_atoms::JsWord;

use super::{
    class::ClassID,
    context::{Context, Variable},
    ir::IR,
    IRContainer, VariableId,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct FunctionId(uuid::Uuid);

impl FunctionId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

impl ToString for FunctionId {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IRFunction {
    pub id: FunctionId,
    pub parent: Option<FunctionId>,

    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub is_class_function: bool,
    pub is_constructor: bool,

    pub heap_variables: Vec<VariableId>,
    pub own_variables: Vec<VariableId>,
    pub captures: Vec<VariableId>,

    pub ir: Vec<IR>,
}

pub struct IRFunctionBuilder<'a> {
    pub id: FunctionId,
    pub ir_module: &'a mut dyn IRContainer,

    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub is_constructor: bool,
    pub class_function: Option<ClassID>,

    pub captures: Vec<VariableId>,

    pub heap_variables: Vec<VariableId>,
    pub own_variable: HashSet<VariableId>,
    pub var_declares: HashMap<JsWord, VariableId>,
    pub bindings: Context<JsWord, VariableId>,
    pub context: Context<JsWord, Variable>,

    pub ir: Vec<IR>,
}

impl<'a> IRFunctionBuilder<'a> {
    pub fn finish(self) -> IRFunction {
        let parent = if self.ir_module.is_function() {
            let f = unsafe {
                (self.ir_module as *mut dyn IRContainer as *mut IRFunctionBuilder)
                    .as_ref()
                    .unwrap()
            };
            Some(f.id)
        } else {
            None
        };

        return IRFunction {
            id: self.id,
            parent,

            is_arrow: self.is_arrow,
            is_async: self.is_async,
            is_generator: self.is_generator,
            is_class_function: self.class_function.is_some(),
            is_constructor: self.is_constructor,

            heap_variables: self.heap_variables,
            own_variables: self.own_variable.into_iter().collect(),
            captures: self.captures,
            ir: self.ir,
        };
    }
}

impl<'a> IRContainer for IRFunctionBuilder<'a> {
    fn _add_heap_variable(&mut self, varid: VariableId) {
        if self.owns_variable(varid) {
            self.heap_variables.push(varid);
        } else {
            self.captures.push(varid);
            self.ir_module._add_heap_variable(varid);
        }
    }

    fn new_context(&mut self) {
        self.bindings.new_layer();
        self.context.new_layer();
    }

    fn close_context(&mut self) {
        self.bindings.remove_layer();
        self.context.remove_layer();
    }

    fn is_class_function(&self) -> Option<ClassID> {
        self.class_function
    }

    fn is_constructor(&self) -> bool {
        self.is_constructor
    }

    fn is_function(&self) -> bool {
        return true;
    }

    fn is_async(&self) -> bool {
        self.is_async
    }

    fn is_generator(&self) -> bool {
        self.is_generator
    }

    fn is_global(&self) -> bool {
        false
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

        if let Some(v) = self.bindings.get(name) {
            return Some(Variable::Binding(*v));
        }

        // find the variable from parent and capture it
        if let Some(v) = self.ir_module.read_var(name) {
            self.captures.push(v.id());
            self.ir_module._add_heap_variable(v.id());
            return Some(v);
        }

        return None;
    }

    fn declare_anonymos(&mut self) -> VariableId {
        let id = VariableId::new();
        self.own_variable.insert(id);
        self.push(IR::DeclareVar(id));
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
