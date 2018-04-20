use interpreter::error::{Error, ErrorKind};
use interpreter::value::{FunctionDef, List, Param, Type, Value, ValueRef};

pub fn get(name: &str, val: ValueRef) -> Option<ValueRef> {
    match name {
        "len" => Some(Value::ExternMethod(Box::new(method_len), val).into()),
        "next" => Some(Value::ExternMethod(Box::new(method_next), val).into()),
        "push" => Some(Value::ExternMethod(Box::new(method_push), val).into()),
        "pop" => Some(Value::ExternMethod(Box::new(method_pop), val).into()),
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

fn method_push(list_val: &ValueRef, args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut val_write = list_val.write().unwrap();

    if args.len() != 1 {
        return Err(Error::new(ErrorKind::InvalidArgumentAmount));
    }

    match &mut *val_write {
        &mut Value::List(List(ref mut vec, ref ty)) => {
            let add_read = args[0].read().unwrap();
            if &add_read.get_type() != ty {
                Err(Error::new(ErrorKind::TypeMismatch(add_read.get_type(), ty.clone(), "cant add 'type1' to 'type2' list".to_owned())))
            } else {
                vec.push(ValueRef::clone(&args[0]));
                
                Ok(Value::Void.into())
            }
        }
        val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "expected 'list'".to_owned()))),
    }
}

fn method_pop(list_val: &ValueRef, args: &[ValueRef]) -> Result<ValueRef, Error> {
    let mut val_write = list_val.write().unwrap();

    match &mut *val_write {
        &mut Value::List(List(ref mut vec, ref ty)) => {
            Ok(Value::Option(vec.pop(), ty.clone()).into())
        }
        val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "expected 'list'".to_owned()))),
    }
}