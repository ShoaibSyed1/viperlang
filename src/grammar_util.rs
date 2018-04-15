use ast::{Class, Field, Function, Method};

pub enum ModuleItem {
    Class(Class),
    Function(Function),
}

pub enum ClassItem {
    Field(Field),
    Function(Function),
    Method(Method),
}