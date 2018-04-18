use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::RwLock;

use ast::Expr;

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Void,

    Function(Rc<Function>),
    Object(Object),
    Class(Rc<Class>),
    Method(Rc<Method>, ValueRef),

    Option(Option<ValueRef>, Type),

    Field(ValueRef, String),
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
            &Value::Object(ref obj) => Type::Object(Rc::clone(&obj.class.def)),
            &Value::Class(ref class) => Type::Class(Rc::clone(&class.def)),
            &Value::Method(ref met, _) => Type::Method(met.0.def.clone()),

            &Value::Option(_, ref ty) => Type::Option(Box::new(ty.clone())),

            &Value::Field(ref val_ref, _) => val_ref.read().unwrap().get_type(),
        }
    }
}

pub type ValueRef = Rc<RwLock<Value>>;
pub type ValueRw = RwLock<Value>;

impl Into<ValueRef> for Value {
    fn into(self) -> ValueRef {
        ValueRef::new(self.into())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    String,
    Void,

    Function(FunctionDef),
    Object(Rc<ClassDef>),
    Class(Rc<ClassDef>),
    Method(FunctionDef),

    Option(Box<Type>),
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
    pub default: Option<ValueRef>,
}

impl PartialEq for Param {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub class: Rc<Class>,
    pub fields: HashMap<String, ValueRef>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub functions: HashMap<String, Rc<Function>>,
    pub methods: HashMap<String, Rc<Method>>,
    pub def: Rc<ClassDef>,
}

#[derive(Clone, Debug)]
pub struct ClassDef {
    pub fields: HashMap<String, Type>,
    pub name: String, // TODO: Proper path support
}

impl PartialEq for ClassDef {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Clone, Debug)]
pub struct Method(pub Function);