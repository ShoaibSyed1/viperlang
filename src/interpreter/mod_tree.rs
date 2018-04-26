use std::collections::HashMap;

use interpreter::environment::{Environment, EnvironmentRef};

pub struct ModuleTree {
    
}

impl ModuleTree {
    pub fn new() -> Self {
        ModuleTree {}
    }
}

pub enum ModuleNode {
    Base {
        children: HashMap<String, ModuleNode>,
        env: EnvironmentRef,
    },
    Leaf {
        env: EnvironmentRef,
    },
}

impl ModuleNode {
    pub fn new_base() -> Self {
        ModuleNode::Base {
            children: HashMap::new(),
            env: Environment::new(None).into(),
        }
    }

    pub fn new_leaf() -> Self {
        ModuleNode::Leaf {
            env: Environment::new(None).into(),
        }
    }
}