use ast::{Class, Field, Function, Method};

pub enum ModuleItem {
    Class(Class),
    Function(Function),
}

pub enum ClassItem {
    StaticField(Field),
    Field(Field),
    Function(Function),
    Method(Method),
}