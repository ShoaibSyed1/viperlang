use std::collections::HashMap;

use interpreter::environment::{Environment, EnvironmentRef};

pub struct ModuleTree {
    
}

impl ModuleTree {
    pub fn new() -> Self {
        ModuleTree {}
    }
}

pub struct ModuleNode {
    pub children: HashMap<String, ModuleNode>,
    pub env: EnvironmentRef,
}

impl ModuleNode {
    pub fn new() -> Self {
        ModuleNode {
            children: HashMap::new(),
            env: Environment::new(None).into(),
        }
    }
}