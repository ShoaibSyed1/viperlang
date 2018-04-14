use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Void,
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            &Value::Boolean(_) => Type::Boolean,
            &Value::Integer(_) => Type::Integer,
            &Value::Float(_) => Type::Float,
            &Value::String(_) => Type::String,
            &Value::Void => Type::Void,
        }
    }
}

pub type ValueRc = Rc<Value>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    String,
    Void,
}

impl Type {
    pub fn is_copy(&self) -> bool {
        match self {
            &Type::Boolean | &Type::Integer | &Type::Float => true,
            _ => false,
        }
    }
}