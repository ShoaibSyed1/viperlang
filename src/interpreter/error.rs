use interpreter::value::{Value, ValueRef, Type};

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Error {
            kind: kind,
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnknownName(String),
    TypeMismatch(Type, Type, String),
    InvalidCast(Type, Value, String),
    InvalidFunction(String),
    InvalidClass(String, String),
    UnknownField(Value, String),
    AssignmentError(Value),
    InvalidLVal,
    CantBreakFunction,
    InvalidIfHas,
    InvalidFor,
    InvalidMacro,
    InvalidList,
    IndexOutOfRange,
    InvalidIndex,
    InvalidArgumentAmount,

    Return(ValueRef),
    Break(ValueRef),
}