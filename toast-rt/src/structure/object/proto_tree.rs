use std::sync::Arc;

use parking_lot::RwLock;

/// a proto tree is a tree structure that
/// describes the structure of an object
pub struct ProtoTree {
    pub root: Arc<ProtoNode>,
}

lazy_static::lazy_static! {
    pub static ref PROTOTREE: ProtoTree = ProtoTree{
        root: Arc::new(ProtoNode{
            parent: None,
            property_name: "__proto__".to_string(),
            property_index: 0,
            branches: RwLock::new(Vec::new())
        })
    };
}

pub struct ProtoNode {
    // proto node will never but free
    // allow circular reference
    pub parent: Option<Arc<ProtoNode>>,
    pub property_name: String,
    pub property_index: usize,
    pub branches: RwLock<Vec<Arc<ProtoNode>>>,
}

impl ProtoNode {
    /// search for a property index
    pub fn search_property(&self, name: &str) -> Option<usize> {
        if self.property_name == name {
            return Some(self.property_index);
        }

        if let Some(parent) = &self.parent {
            //if let Some(parent) = parent.upgrade(){
            return parent.search_property(name);
            //}
        }

        return None;
    }

    /// add property to the tree and return the leaf node
    pub fn add_property(self: Arc<ProtoNode>, name: &str) -> Arc<ProtoNode> {
        //let weak_self = Arc::downgrade(&self);

        // try to find branch that satisfies property
        let mut branches = self.branches.upgradable_read();
        // loop through branches
        for branch in branches.iter() {
            // compare names
            if branch.property_name == name {
                // return the already created branch
                return branch.clone();
            }
        }

        // create a new node
        let node = Arc::new(ProtoNode {
            parent: Some(self.clone()),
            property_name: name.to_string(),
            property_index: self.property_index + 1,
            branches: RwLock::new(Vec::new()),
        });

        branches.with_upgraded(|branches| branches.push(node.clone()));

        return node;
    }
}
