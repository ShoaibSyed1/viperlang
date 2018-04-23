use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

use interpreter::error::{Error, ErrorKind};
use interpreter::value::ValueRef;

#[derive(Clone)]
pub struct Environment {
    parent: Option<EnvironmentRef>,
    variables: HashMap<String, ValueRef>,
}

impl Environment {
    pub fn new(parent: Option<EnvironmentRef>) -> Self {
        Environment {
            parent: parent,
            variables: HashMap::new(),
        }
    }

    pub fn create(&mut self, name: String, value: ValueRef) -> Result<(), Error> {
        self.variables.insert(name, value);

        Ok(())
    }

    pub fn assign(&mut self, name: String, value: ValueRef) -> Result<(), Error> {
        {
            let cur_variable = self.variables.get(&name);
            if cur_variable.is_none() {
                if let Some(ref mut env) = self.parent {
                    return env.write().unwrap().assign(name, value);
                }
            }
            
            let cur_variable = cur_variable.ok_or(Error::new(ErrorKind::UnknownName(name.clone())))?;

            let cur_variable = cur_variable.read().unwrap();
            let value = value.read().unwrap();
            
            if cur_variable.get_type() != value.get_type() {
                return Err(Error::new(ErrorKind::TypeMismatch(cur_variable.get_type(), value.get_type(), "cannot assign 'type2' to 'type1' variable".into())));
            }
        }

        self.variables.insert(name.clone(), value);

        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<ValueRef> {
        let mut val = self.variables.get(name).map(|v| ValueRef::clone(v));
        if val.is_none() {
            if let Some(ref env) = self.parent {
                val = env.read().unwrap().get(name);
            }
        }
        
        val
    }

    pub fn get_parent(&mut self) -> Option<EnvironmentRef> {
        ::std::mem::replace(&mut self.parent, None)
    }
}

pub type EnvironmentRef = Rc<RwLock<Environment>>;

impl Into<EnvironmentRef> for Environment {
    fn into(self) -> EnvironmentRef {
        Rc::new(RwLock::new(self))
    }
}