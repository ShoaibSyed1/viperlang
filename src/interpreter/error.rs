use interpreter::value::{Value, Type};

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
}