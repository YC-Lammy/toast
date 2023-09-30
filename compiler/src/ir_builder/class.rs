use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassID(uuid::Uuid);

impl ClassID {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRClass {
    id: ClassID,
    constructor: Option<FunctionId>,
    public_props: Vec<JsWord>,
    private_props: Vec<JsWord>,
    static_props: Vec<JsWord>,
    getters: HashMap<JsWord, FunctionId>,
    setters: HashMap<JsWord, FunctionId>,
    accessors: Vec<JsWord>,
    static_block: Option<Vec<IR>>,
}

impl IRBuilder {
    pub fn translate_class(
        &mut self,
        container: &mut dyn IRContainer,
        class: &Class,
        classid: ClassID,
    ) -> Result<(), Error> {
        let mut super_class = None;

        if let Some(e) = &class.super_class {
            self.translate_expr(container, e)?;
            let id = TempId::new();
            super_class = Some(id);
            container.push(IR::StoreTemp(id));
        }

        for member in &class.body {
            match member {
                ClassMember::Empty(e) => {}
                ClassMember::Constructor(c) => {}
                ClassMember::ClassProp(p) => {}
                ClassMember::PrivateProp(p) => {}
                ClassMember::Method(m) => {}
                ClassMember::PrivateMethod(m) => {}
                ClassMember::AutoAccessor(a) => {}
                ClassMember::StaticBlock(s) => {}
                ClassMember::TsIndexSignature(s) => {}
            }
        }

        if let Some(id) = super_class {
            container.push(IR::DropTemp(id));
        }

        return Ok(());
    }
}
