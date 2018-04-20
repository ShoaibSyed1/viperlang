use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::RwLock;

use ast::Expr;
use interpreter::error::Error;

#[derive(Clone)]
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

    ExternMethod(Box<fn(&ValueRef, &[ValueRef]) -> Result<ValueRef, Error>>, ValueRef),

    Option(Option<ValueRef>, Type),
    List(List),

    Field(ValueRef, String),
    Index(ValueRef, ValueRef),
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

            &Value::ExternMethod(_, _) => Type::ExternMethod,

            &Value::Option(_, ref ty) => Type::Option(Box::new(ty.clone())),
            &Value::List(List(_, ref ty)) => Type::List(Box::new(ty.clone())),

            &Value::Field(ref val_ref, _) => val_ref.read().unwrap().get_type(),
            &Value::Index(ref val, _) => val.read().unwrap().get_type(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Value::{}",
            match self {
                &Value::Boolean(_) => "Boolean",
                &Value::Integer(_) => "Integer",
                &Value::Float(_) => "Float",
                &Value::String(_) => "String",
                &Value::Void => "Void",

                &Value::Function(_) => "Function",
                &Value::Object(_) => "Object",
                &Value::Class(_) => "Class",
                &Value::Method(_, _) => "Method",

                &Value::ExternMethod(_, _) => "ExternMethod",

                &Value::Option(_, _) => "Option",
                &Value::List(_) => "List",

                &Value::Field(_, _) => "Field",
                &Value::Index(_, _) => "Index",
            }
        )
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

    ExternMethod,

    Option(Box<Type>),
    List(Box<Type>),
}

impl Type {
    pub fn is_copy(&self) -> bool {
        match self {
            &Type::Boolean | &Type::Integer | &Type::Float => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct List(pub Vec<ValueRef>, pub Type);

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