use std::cmp::PartialEq;
use std::fmt;
use std::rc::Rc;

use ast::Expr;

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Void,

    Function(Function),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            &Value::Boolean(_) => Type::Boolean,
            &Value::Integer(_) => Type::Integer,
            &Value::Float(_) => Type::Float,
            &Value::String(_) => Type::String,
            &Value::Void => Type::Void,

            &Value::Function(ref func) => Type::Function(func.def.clone()),
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

    Function(FunctionDef),
}

impl Type {
    pub fn is_copy(&self) -> bool {
        match self {
            &Type::Boolean | &Type::Integer | &Type::Float => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub def: FunctionDef,
    pub body: Expr,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Function {{ def: {:?}, ... }}", self.def)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef {
    pub params: Vec<Param>,
    pub ret: Box<Type>,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub default: Option<ValueRc>,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}