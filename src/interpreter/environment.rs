use std::collections::HashMap;

use interpreter::error::{Error, ErrorKind};
use interpreter::value::ValueRef;

pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, ValueRef>,
}

impl Environment {
    pub fn new(parent: Option<Box<Environment>>) -> Self {
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
                    return env.assign(name, value);
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
                val = env.get(name);
            }
        }
        
        val
    }

    pub fn get_parent(&mut self) -> Option<Box<Self>> {
        ::std::mem::replace(&mut self.parent, None)
    }
}