use interpreter::error::{Error, ErrorKind};
use interpreter::value::{FunctionDef, List, Param, Type, Value, ValueRef};

pub fn get(name: &str, val: ValueRef) -> Option<ValueRef> {
    match name {
        "len" => Some(Value::ExternMethod(Box::new(method_len), val).into()),
        "next" => Some(Value::ExternMethod(Box::new(method_next), val).into()),
        _ => None,
    }
}

fn method_len(list_val: &ValueRef, args: &[ValueRef]) -> Result<ValueRef, Error> {
    let len = match &*list_val.read().unwrap() {
        &Value::List(List(ref vec, _)) => vec.len(),
        val => return Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "expected 'list'".to_owned()))),
    };

    Ok(Value::Integer(len as _).into())
}

fn method_next(list_val: &ValueRef, args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut val_write = list_val.write().unwrap();

    match &mut *val_write {
        &mut Value::List(List(ref mut vec, ref ty)) => {
            if vec.len() > 0 {
                Ok(Value::Option(Some(vec.remove(0)), ty.clone()).into())
            } else {
                Ok(Value::Option(None, ty.clone()).into())
            }
        }
        val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "expected 'list'".to_owned()))),
    }
}