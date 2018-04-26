use ast::{Class, Field, Function, Method, Mod};

pub enum ModuleItem {
    Class(Class),
    Function(Function),
    Mod(Mod),
}

pub enum ClassItem {
    StaticField(Field),
    Field(Field),
    Function(Function),
    Method(Method),
}