use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use ast::{Arg, BinOp, Block, Class as AstClass, Expr, Function as AstFunction, ItemPath, ListInit, Literal, MakeArg, Module, Stmt, Type as AstType, UnOp};

mod environment;
mod error;
mod e_list;
mod mod_tree;
mod value;

use self::environment::{Environment, EnvironmentRef};
use self::error::{Error, ErrorKind};
use self::mod_tree::ModuleNode;
use self::value::{Class, ClassDef, Function, FunctionDef, List, Method, Object, Param, Type, Value, ValueRef, ValueRw};

pub struct Interpreter {
    env: EnvironmentRef,

    mod_tree: ModuleNode,
    cur_path: Vec<String>,
}

impl Interpreter {
    pub fn new(mod_tree: ModuleNode) -> Self {
        Interpreter {
            env: Environment::new(None).into(),

            mod_tree: mod_tree,
            cur_path: Vec::new(),
        }
    }

    pub fn eval_module(&mut self, module: &Module, call_main: bool) -> Result<(), Error> {
        for class in &module.classes {
            self.convert_class(class)?;
        }

        for function in &module.functions {
            self.convert_function(function)?;
        }

        if call_main {
            if let Some(value) = self.get("main") {
                self.call(&value, &[])?;
            }
        }

        Ok(())
    }

    fn eval_block(&mut self, block: &Block) -> Result<ValueRef, Error> {
        let env = Environment::new(Some(::std::mem::replace(&mut self.env, Environment::new(None).into()))).into();
        self.env = env;

        for stmt in &block.0 {
            self.eval_stmt(stmt)?;
        }

        let ret = self.eval_expr(&block.1)?;

        let new_env = self.env
            .write()
            .unwrap()
            .get_parent()
            .unwrap_or_else(|| EnvironmentRef::new(Environment::new(None).into()));

        self.env = new_env;
        
        Ok(ret)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            &Stmt::Let(ref name, ref opt_type, ref expr) => {
                let val = self.eval_expr(expr)?;

                if let Some(ref ty) = opt_type {
                    let ty = self.ast_type_to_runtime_type(ty)?;

                    let val_read = val.read().unwrap();

                    if ty != val_read.get_type() {
                        return Err(Error::new(ErrorKind::TypeMismatch(ty, val_read.get_type(), "cannot assign 'type2' to 'type1' variable".into())));
                    }
                }

                self.create(name.to_owned(), val)
            }
            &Stmt::Assign(ref lexpr, ref rexpr) => {
                let val = self.eval_expr(rexpr)?;

                match lexpr {
                    &Expr::Literal(Literal::Ident(ref path)) => {
                        if path.0.len() != 1 {
                            return Err(Error::new(ErrorKind::InvalidLVal));
                        } else {
                            self.assign(path.0.get(0).unwrap().to_owned(), val)?;
                        }
                    }
                    &Expr::Dot(ref dlexpr, ref ident) => {
                        let dlval = self.eval_expr(dlexpr)?;

                        let lval = self.dot(dlval, ident, true)?;

                        let lval = lval.read().unwrap();

                        // Special case
                        match &*lval {
                            &Value::Field(ref val_ref, ref name) => {
                                match &mut *val_ref.write().unwrap() {
                                    &mut Value::Object(ref mut obj) => {
                                        obj.fields.insert(name.clone(), val);
                                    }
                                    _ => return Err(Error::new(ErrorKind::InvalidLVal)),
                                }
                            }
                            _ => return Err(Error::new(ErrorKind::InvalidLVal)),
                        }
                    }
                    &Expr::Index(ref ilexpr, ref vexpr) => {
                        let ilval = self.eval_expr(ilexpr)?;
                        let ival = self.eval_expr(vexpr)?;

                        let lval = self.index(&ilval, &ival, true)?;

                        let lval = lval.read().unwrap();

                        match &*lval {
                            &Value::Index(ref val_ref, ref ival_ref) => {
                                let index = match &*ival_ref.read().unwrap() {
                                    &Value::Integer(val) => val,
                                    _ => return Err(Error::new(ErrorKind::InvalidIndex)),
                                };

                                match &mut *val_ref.write().unwrap() {
                                    &mut Value::List(List(ref mut v, ref ty)) => {
                                        let vty = val.read().unwrap().get_type();
                                        if &vty != ty {
                                            return Err(Error::new(ErrorKind::TypeMismatch(vty, ty.clone(), "cannot add 'type2' to 'type1' list".to_owned())))
                                        } else {
                                            v[index as usize] = val;
                                        }
                                    },
                                    _ => panic!("HERE 1"),
                                }
                            }
                            _ => panic!("HERE 2"),
                        }
                    },
                    _ => panic!("HERE 3"),
                }

                Ok(())
            }
            &Stmt::Expr(ref expr) => {
                self.eval_expr(expr)?;

                Ok(())
            }

            &Stmt::Return(ref expr) => {
                let val = self.eval_expr(expr)?;

                Err(Error::new(ErrorKind::Return(val)))
            }
            &Stmt::Break(ref expr) => {
                let val = self.eval_expr(expr)?;

                Err(Error::new(ErrorKind::Break(val)))
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<ValueRef, Error> {
        match expr {
            &Expr::UnOp(ref op, ref e) => {
                let v = self.eval_expr(e)?;

                self.do_unop(op, &v)
            }
            &Expr::BinOp(ref e1, ref op, ref e2) => {
                let v1 = self.eval_expr(e1)?;
                let v2 = self.eval_expr(e2)?;

                self.do_binop(&v1, op, &v2)
            },
            &Expr::Literal(ref literal) => self.eval_literal(literal),

            &Expr::Block(ref block) => self.eval_block(block),            

            &Expr::If(ref cond, ref block, ref else_block) => self.eval_if(cond, block, else_block),
            &Expr::IfHas(ref expr, ref ident, ref block, ref else_block) => {
                let val = self.eval_expr(expr)?;

                self.eval_if_has(&val, ident, block, else_block)
            }
            &Expr::While(ref cond, ref block) => self.eval_while(cond, block),
            &Expr::For(ref ident, ref expr, ref block) => {
                let val = self.eval_expr(expr)?;

                self.eval_for(ident, &val, block)
            }

            &Expr::Call(ref expr, ref args) => {
                let callable = self.eval_expr(expr)?;
                self.call(&callable, args)
            }

            &Expr::Make(ref expr, ref args) => {
                let makeable = self.eval_expr(expr)?;
                self.make(&makeable, args)
            }
            &Expr::MakeList(ref ty, ref list_init) => {
                let ty = self.ast_type_to_runtime_type(ty)?;

                let mut values = Vec::new();

                match list_init {
                    &ListInit::Items(ref items) => {
                        for item in items {
                            let val = self.eval_expr(item)?;

                            if &val.read().unwrap().get_type() == &ty {
                                values.push(val);
                            } else {
                                return Err(Error::new(ErrorKind::InvalidList));
                            }
                        }
                    }
                    &ListInit::Duplicate(ref expr, amount) => {
                        for _ in 0..amount {
                            values.push(self.eval_expr(expr)?);
                        }
                    }
                }

                Ok(Value::List(List(values, ty)).into())
            }

            &Expr::Dot(ref expr, ref ident) => {
                let val = self.eval_expr(expr)?;
                self.dot(val, ident, false)
            }

            &Expr::Cast(ref ast_type, ref expr) => {
                let val = self.eval_expr(expr)?;
                let ty = self.ast_type_to_runtime_type(ast_type)?;

                self.do_cast(&ty, &val)
            }

            &Expr::OptionSome(ref expr) => {
                let val = self.eval_expr(expr)?;

                let ty = val.read().unwrap().get_type();

                Ok(Value::Option(Some(val), ty).into())
            }

            &Expr::Macro(ref ident, ref args) => {
                match ident as &str {
                    "print" => self.macro_print(args),
                    "println" => self.macro_println(args),
                    "readln" => self.macro_readln(args),
                    "len" => self.macro_len(args),
                    _ => Err(Error::new(ErrorKind::InvalidMacro)),
                }
            }

            &Expr::Index(ref expr, ref index) => {
                let val = self.eval_expr(expr)?;
                let index = self.eval_expr(index)?;

                self.index(&val, &index, false)
            }
        }
    }

    fn eval_if(&mut self, cond: &Expr, block: &Block, else_block: &Option<Block>) -> Result<ValueRef, Error> {
        let cond = self.eval_expr(cond)?;
        let cond = match &*cond.read().unwrap() {
            &Value::Boolean(b) => b,
            other => return Err(Error::new(ErrorKind::TypeMismatch(Type::Boolean, other.get_type(), "expected type 'bool' in if condition".to_owned()))),
        };

        if cond {
            self.eval_block(block)
        } else {
            if let Some(ref block) = else_block.as_ref() {
                self.eval_block(block)
            } else {
                Ok(ValueRef::new(Value::Void.into()))
            }
        }
    }

    fn eval_if_has(&mut self, val: &ValueRef, name: &str, block: &Block, else_block: &Option<Block>) -> Result<ValueRef, Error> {
        let env = Environment::new(Some(::std::mem::replace(&mut self.env, Environment::new(None).into()))).into();
        self.env = env;

        match &*val.read().unwrap() {
            &Value::Option(ref opt, _) => {
                match opt {
                    Some(val) => {
                        self.create(name.to_owned(), ValueRef::clone(val))?;

                        return self.eval_block(block);
                    }
                    None => {
                        if let Some(else_block) = else_block {
                            return self.eval_block(else_block);
                        }
                    }
                }
            }
            _ => return Err(Error::new(ErrorKind::InvalidIfHas)),
        }

        let new_env = self.env
            .write()
            .unwrap()
            .get_parent()
            .unwrap_or_else(|| EnvironmentRef::new(Environment::new(None).into()));

        self.env = new_env;

        Ok(Value::Void.into())
    }

    fn eval_while(&mut self, cond: &Expr, block: &Block) -> Result<ValueRef, Error> {
        let mut ret = ValueRef::new(Value::Void.into());

        loop {
            let cond = self.eval_expr(cond)?;
            let cond = match &*cond.read().unwrap() {
                &Value::Boolean(b) => b,
                other => return Err(Error::new(ErrorKind::TypeMismatch(Type::Boolean, other.get_type(), "expected type 'bool' in while condition".to_owned()))),
            };

            if cond {
                match self.eval_block(block) {
                    Ok(val) => ret = val,
                    Err(Error { kind: ErrorKind::Break(val) }) => return Ok(val),
                    Err(err) => return Err(err),
                }
            } else {
                break;
            }
        }

        Ok(ret)
    }

    fn eval_for(&mut self, name: &str, val: &ValueRef, block: &Block) -> Result<ValueRef, Error> {
        let env = Environment::new(Some(::std::mem::replace(&mut self.env, Environment::new(None).into()))).into();
        self.env = env;

        let method = self.get_method(val, "next").ok_or(Error::new(ErrorKind::InvalidFor))?;        

        let mut ret = ValueRef::new(Value::Void.into());

        loop {
            let val = self.call(&method, &[])?;
            match &*val.read().unwrap() {
                &Value::Option(ref opt, _) => match opt {
                    Some(val) => self.create(name.to_owned(), ValueRef::clone(val))?,
                    _ => break,
                },
                _ => return Err(Error::new(ErrorKind::InvalidIfHas)),
            }

            match self.eval_block(block) {
                Ok(val) => ret = val,
                Err(Error { kind: ErrorKind::Break(val) }) => {
                    ret = val;
                    break;
                }
                Err(err) => return Err(err),
            }
        }

        let new_env = self.env
            .write()
            .unwrap()
            .get_parent()
            .unwrap_or_else(|| EnvironmentRef::new(Environment::new(None).into()));

        self.env = new_env;

        Ok(ret)
    }

    fn eval_literal(&self, literal: &Literal) -> Result<ValueRef, Error> {
        match literal {
            &Literal::Boolean(val) => Ok(ValueRef::new(Value::Boolean(val).into())),
            &Literal::Integer(val) => Ok(ValueRef::new(Value::Integer(val).into())),
            &Literal::Float(val) => Ok(ValueRef::new(Value::Float(val).into())),
            &Literal::String(ref val) => Ok(ValueRef::new(Value::String(val.to_owned()).into())),
            &Literal::Ident(ref item_path) => {
                self.get_from_path(&item_path)
            }
            &Literal::Void => Ok(ValueRef::new(Value::Void.into())),
            &Literal::None(ref ty) => Ok(Value::Option(None, self.ast_type_to_runtime_type(ty)?).into()),
        }
    }

    fn do_unop(&self, op: &UnOp, val: &ValueRw) -> Result<ValueRef, Error> {
        match op {
            &UnOp::Neg => self.do_neg(val),
            &UnOp::Not => self.do_not(val),
        }
    }

    fn do_neg(&self, val: &ValueRw) -> Result<ValueRef, Error> {
        match &*val.read().unwrap() {
            &Value::Integer(i) => Ok(ValueRef::new(Value::Integer(-i).into())),
            &Value::Float(f) => Ok(ValueRef::new(Value::Float(-f).into())),
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "no implementation of 'neg' for this type".to_owned()))),
        }
    }

    fn do_not(&self, val: &ValueRw) -> Result<ValueRef, Error> {
        match &*val.read().unwrap() {
            &Value::Boolean(b) => Ok(ValueRef::new(Value::Boolean(!b).into())),
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "no implementation of 'not' for this type".to_owned()))),
        }
    }

    fn do_binop(&self, l: &ValueRw, op: &BinOp, r: &ValueRw) -> Result<ValueRef, Error> {
        match op {
            &BinOp::Add => self.do_add(l, r),
            &BinOp::Sub => self.do_sub(l, r),
            &BinOp::Mul => self.do_mul(l, r),
            &BinOp::Div => self.do_div(l, r),

            &BinOp::Modulo => self.do_modulo(l, r),

            &BinOp::Eq => self.do_eq(l, r),
            &BinOp::Neq => self.do_neq(l, r),
            
            &BinOp::Gt => self.do_gt(l, r),
            &BinOp::Gteq => self.do_gteq(l, r),
            &BinOp::Lt => self.do_lt(l, r),
            &BinOp::Lteq => self.do_lteq(l, r),

            &BinOp::Or => self.do_or(l, r),
            &BinOp::And => self.do_and(l, r),
        }
    }

    fn do_add(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(ValueRef::new(Value::Integer(i1 + i2).into())),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(ValueRef::new(Value::Float(f1 + f2).into())),
            (&Value::String(ref s1), &Value::String(ref s2)) => Ok(ValueRef::new(Value::String(s1.to_owned() + s2).into())),
            (&Value::List(List(ref v1, ref t1)), &Value::List(List(ref v2, ref t2))) => {
                if t1 == t2 {
                    let mut v = Vec::with_capacity(v1.len() + v2.len());
                    v.extend_from_slice(v1);
                    v.extend_from_slice(v2);

                    Ok(Value::List(List(v, t1.clone())).into())
                } else {
                    Err(Error::new(ErrorKind::TypeMismatch(t1.clone(), t2.clone(), "cannot add two lists of different types".to_owned())))
                }
            },
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'add' for these types".to_owned()))),
        }
    }

    fn do_sub(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 - i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 - f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'sub' for these types".to_owned()))),
        }
    }

    fn do_mul(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 * i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 * f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'mul' for these types".to_owned()))),
        }
    }

    fn do_div(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 / i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 / f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'div' for these types".to_owned()))),
        }
    }

    fn do_modulo(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 % i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 % f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'div' for these types".to_owned()))),
        }
    }

    fn do_eq(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Boolean(b1), &Value::Boolean(b2)) => Ok(Value::Boolean(b1 == b2).into()),
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 == i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 == f2).into()),
            (&Value::String(ref s1), &Value::String(ref s2)) => Ok(Value::Boolean(s1 == s2).into()),
            (&Value::Option(ref val1, ref ty1), &Value::Option(ref val2, ref ty2)) => {
                let ty_comp = ValueRef::new(Value::Boolean(ty1 == ty2).into());
                let val_comp = if let (Some(val1), Some(val2)) = (val1, val2) {
                    self.do_eq(&val1, &val2)?
                } else {
                    ValueRef::new(Value::Boolean(val1.is_none() && val2.is_none()).into())
                };

                self.do_and(&ty_comp, &val_comp)
            }
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'eq' for these types".to_owned()))),
        }
    }

    fn do_neq(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        self.do_eq(val1, val2).map(|v| if let &Value::Boolean(b) = &*v.read().unwrap() {
            Value::Boolean(!b).into()
        } else { v.clone() })
    }

    fn do_gt(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 > i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 > f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'gt' for these types".to_owned()))),
        }
    }

    fn do_gteq(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 >= i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 >= f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'gteq' for these types".to_owned()))),
        }
    }

    fn do_lt(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 < i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 < f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'lt' for these types".to_owned()))),
        }
    }

    fn do_lteq(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 <= i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 <= f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'lteq' for these types".to_owned()))),
        }
    }

    fn do_or(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Boolean(b1), &Value::Boolean(b2)) => Ok(Value::Boolean(b1 || b2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'or' for these types".to_owned()))),
        }
    }

    fn do_and(&self, val1: &ValueRw, val2: &ValueRw) -> Result<ValueRef, Error> {
        match (&*val1.read().unwrap(), &*val2.read().unwrap()) {
            (&Value::Boolean(b1), &Value::Boolean(b2)) => Ok(Value::Boolean(b1 && b2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'and' for these types".to_owned()))),
        }
    }

    fn do_cast(&self, ty: &Type, val: &ValueRw) -> Result<ValueRef, Error> {
        use std::str::FromStr;

        let val_read = val.read().unwrap();

        if &val_read.get_type() == ty { return Ok(val_read.clone().into()); }

        match (ty, &*val_read) {
            (&Type::Float, &Value::Integer(i)) => Ok(Value::Float(i as f64).into()),
            (&Type::Float, &Value::String(ref s)) => Ok(Value::Float(match f64::from_str(s) {
                Ok(f) => f,
                Err(_) => return Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val_read.clone(), "invalid value".to_owned()))),
            }).into()),

            (&Type::Integer, &Value::Float(f)) => Ok(Value::Integer(f as i64).into()),
            (&Type::Integer, &Value::String(ref s)) => Ok(Value::Integer(match i64::from_str(s) {
                Ok(i) => i,
                Err(_) => return Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val_read.clone(), "invalid value".to_owned()))),
            }).into()),

            (&Type::String, &Value::Integer(i)) => Ok(Value::String(i.to_string()).into()),
            (&Type::String, &Value::Float(f)) => Ok(Value::String(f.to_string()).into()),
            (&Type::String, &Value::Boolean(b)) => Ok(Value::String(b.to_string()).into()),
            (&Type::String, &Value::Void) => Ok(Value::String("void".to_owned()).into()),

            (ty, val) => Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val.clone(), "cast not supported between these types".to_owned()))),
        }
    }

    fn call(&mut self, val: &ValueRw, args: &[Arg]) -> Result<ValueRef, Error> {
        let val_read = val.read().unwrap();
        let err = Error::new(ErrorKind::TypeMismatch(val_read.get_type(), val_read.get_type(), "no implementation of 'call' for this type".to_owned()));

        let mut vals = Vec::new();
        for arg in args {
            vals.push(self.eval_expr(&arg.expr)?);
        }

        match &*val_read {
            &Value::ExternMethod(ref func, ref val) => return func(val, &vals),
            _ => {},
        }

        if vals.len() != (match &*val_read {
            &Value::Function(ref func) => func.def.params.len(),
            &Value::Method(ref method, _) => method.0.def.params.len(),
            _ => return Err(err),
        }) {
            panic!("num args did not match num params");
        }

        let env = Environment::new(Some(::std::mem::replace(&mut self.env, Environment::new(None).into()))).into();
        self.env = env;

        let func = match &*val_read {
            &Value::Function(ref func) => func,
            &Value::Method(ref method, ref obj) => {
                self.create("self".to_owned(), obj.clone().into())?;

                &method.0
            },
            _ => return Err(err),
        };

        for i in 0..(func.def.params.len()) {
            self.create(func.def.params[i].name.clone(), vals[i].clone())?;
        }

        let ret = match self.eval_expr(&func.body) {
            Ok(val) => val,
            Err(Error { kind: ErrorKind::Return(val) }) => val,
            Err(Error { kind: ErrorKind::Break(_) }) => return Err(Error::new(ErrorKind::CantBreakFunction)),
            Err(err) => return Err(err),
        };

        let ret_read = ret.read().unwrap();

        if &ret_read.get_type() != &*func.def.ret {
            return Err(Error::new(ErrorKind::TypeMismatch(ret_read.get_type(), (&*func.def.ret).clone(), "tried to return 'type1' from function with return type 'type2'".to_owned())));
        }

        let new_env = self.env
            .write()
            .unwrap()
            .get_parent()
            .unwrap_or_else(|| EnvironmentRef::new(Environment::new(None).into()));

        self.env = new_env;

        Ok(ret.clone())
    }

    fn make(&mut self, makeable: &ValueRw, args: &[MakeArg]) -> Result<ValueRef, Error> {
        let makeable_read = makeable.read().unwrap();
        let class = match &*makeable_read {
            &Value::Class(ref class) => class,
            _ => return Err(Error::new(ErrorKind::TypeMismatch(makeable_read.get_type(), makeable_read.get_type(), "no implementation of 'make' for this type".to_owned()))),
        };

        let mut fields = HashMap::new();

        for arg in args {
            let val = self.eval_expr(&arg.expr)?;
            let val_read = val.read().unwrap();
            match class.def.fields.get(&arg.name) {
                Some(ty) => {
                    if &val_read.get_type() == ty {
                        fields.insert(arg.name.clone(), val.clone());
                    } else {
                        return Err(Error::new(ErrorKind::TypeMismatch(val_read.get_type(), ty.clone(), "class has 'type2' for field, not 'type1'".to_owned())));
                    }
                },
                None => return Err(Error::new(ErrorKind::UnknownName(format!("class has no field '{}'", arg.name)))),
            }
        }

        for key in class.def.fields.keys() {
            if !fields.contains_key(key) {
                return Err(Error::new(ErrorKind::InvalidClass("".to_owned(), format!("no initializer for '{} 'found", key))));
            }
        }

        Ok(Value::Object(Object {
            class: Rc::clone(class),
            fields: fields,
        }).into())
    }

    fn dot(&mut self, val: ValueRef, ident: &str, ret_field: bool) -> Result<ValueRef, Error> {
        let val_read = val.read().unwrap();
        match &*val_read {
            &Value::Object(ref obj) => {
                match obj.fields.get(ident) {
                    Some(nval) => {
                        if !ret_field {
                            Ok(ValueRef::clone(nval))
                        } else {
                            Ok(Value::Field(val.clone(), ident.to_owned()).into())
                        }
                    }
                    None => {
                        match obj.class.methods.get(ident) {
                            Some(method) => Ok(Value::Method(Rc::clone(method), val.clone()).into()),
                            None => Err(Error::new(ErrorKind::UnknownField((&*val_read).clone(), ident.to_owned())))
                        }
                    }
                }
            }
            &Value::Class(ref class) => {
                match class.functions.get(ident) {
                    Some(func) => Ok(Value::Function(Rc::clone(func)).into()),
                    None => Err(Error::new(ErrorKind::UnknownField((&*val_read).clone(), ident.to_owned()))),
                }
            }
            &Value::List(_) => self::e_list::get(ident, ValueRef::clone(&val))
                .ok_or(Error::new(ErrorKind::UnknownField((&*val_read).clone(), ident.to_owned()))),
            other => Err(Error::new(ErrorKind::TypeMismatch(other.get_type(), other.get_type(), "cannot access members of type 'type1'".to_owned()))),
        }
    }

    fn index(&mut self, val: &ValueRef, index_val: &ValueRef, ret_index: bool) -> Result<ValueRef, Error> {
        let index = match &*index_val.read().unwrap() {
            &Value::Integer(val) => val as usize,
            _ => return Err(Error::new(ErrorKind::InvalidIndex)),
        };

        match &*val.read().unwrap() {
            &Value::List(List(ref vec, _)) => {
                let ret_val = vec.get(index as usize).map(|v| ValueRef::clone(v)).ok_or(Error::new(ErrorKind::IndexOutOfRange))?;
                if ret_index {
                    Ok(Value::Index(ValueRef::clone(val), ValueRef::clone(index_val)).into())
                } else {
                    Ok(ret_val)
                }
            }
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "type does not implement 'index'".to_owned()))),
        }
    }

    fn ast_type_to_runtime_type(&self, ast_type: &AstType) -> Result<Type, Error> {
        match ast_type {
            &AstType::Boolean => Ok(Type::Boolean),
            &AstType::Integer => Ok(Type::Integer),
            &AstType::Float => Ok(Type::Float),
            &AstType::String => Ok(Type::String),
            &AstType::Class(ref name) => {
                let val = self.get(name)
                    .ok_or(Error::new(ErrorKind::InvalidClass(name.to_owned(), "no such class found".to_owned())))?;
                
                let val: &Value = &*val.read().unwrap();
                match val {
                    &Value::Class(ref class) => {
                        Ok(Type::Object(Rc::clone(&class.def)))
                    },
                    _ => {
                        Err(Error::new(ErrorKind::InvalidClass(name.to_owned(), "it is not of type 'class'".to_owned())))
                    }
                }
            }
            &AstType::Void => Ok(Type::Void),
            &AstType::Option(ref aty) => Ok(Type::Option(Box::new(self.ast_type_to_runtime_type(aty)?))),
            &AstType::List(ref aty) => Ok(Type::List(Box::new(self.ast_type_to_runtime_type(aty)?))),
        }
    }

    fn ast_function_to_runtime_function(&self, function: &AstFunction) -> Result<Function, Error> {
        let mut params = Vec::new();

        for ast_param in &function.params {
            let default = if let Some(ref literal) = ast_param.default {
                if let &Literal::Ident(_) = literal {
                    return Err(Error::new(ErrorKind::InvalidFunction("cannot use identifier as default parameter".to_owned())));
                } else {
                    Some(self.eval_literal(literal)?)
                }
            } else { None };

            params.push(Param {
                name: ast_param.name.clone(),
                ty: self.ast_type_to_runtime_type(&ast_param.ty)?,
                default: default,
            });
        }

        let ret = self.ast_type_to_runtime_type(&function.ret)?;

        let body = function.body.clone();

        Ok(Function {
            def: FunctionDef {
                params: params,
                ret: Box::new(ret),
            },
            body: body,
        })
    }

    fn convert_function(&mut self, function: &AstFunction) -> Result<(), Error> {
        let runtime_func = self.ast_function_to_runtime_function(function)?;

        self.create(function.name.clone(), Value::Function(runtime_func.into()).into())?;

        Ok(())
    }

    fn convert_class(&mut self, ast_class: &AstClass) -> Result<(), Error> {
        let temp_class = Class {
            functions: HashMap::new(),
            methods: HashMap::new(),
            def: Rc::new(ClassDef {
                fields: HashMap::new(),
                name: ast_class.name.to_owned(),
            }),
        };

        let value = ValueRef::new(ValueRw::new(Value::Class(Rc::new(temp_class))));

        self.create(ast_class.name.clone(), ValueRef::clone(&value))?;

        let mut field_map = HashMap::new();

        for field in &ast_class.fields {
            field_map.insert(field.name.clone(), self.ast_type_to_runtime_type(&field.ty)?);
        }

        let mut method_map = HashMap::new();

        for ast_method in &ast_class.methods {
            let runtime_method = Method(self.ast_function_to_runtime_function(&ast_method.0)?);
            
            method_map.insert(ast_method.0.name.clone(), Rc::new(runtime_method));
        }

        let mut function_map = HashMap::new();

        for ast_function in &ast_class.functions {
            let runtime_function = self.ast_function_to_runtime_function(ast_function)?;

            function_map.insert(ast_function.name.clone(), Rc::new(runtime_function));
        }

        let class = Class {
            functions: function_map,
            methods: method_map,
            def: Rc::new(ClassDef {
                fields: field_map,
                name: ast_class.name.to_owned(),
            }),
        };

        *value.write().unwrap() = Value::Class(Rc::new(class));

        Ok(())
    }

    fn get_method(&self, val: &ValueRef, name: &str) -> Option<ValueRef> {
        match &*val.read().unwrap() {
            &Value::Object(ref obj) => {
                obj.class.methods.get(name).map(|m| ValueRef::new(Value::Method(Rc::clone(m), ValueRef::clone(val)).into()))
            }
            &Value::List(_) => self::e_list::get(name, ValueRef::clone(val)),
            _ => None,
        }
    }

    fn macro_print(&mut self, args: &[Arg]) -> Result<ValueRef, Error> {
        use std::io::{self, Write};

        let mut vals = Vec::new();
        for arg in args {
            vals.push(self.eval_expr(&arg.expr)?);
        }

        let stdout = io::stdout();
        let mut handle = stdout.lock();

        let mut counter = 0;
        let vals_len = vals.len();

        for val in vals {
            match self.do_cast(&Type::String, &val) {
                Ok(val) => {
                    match &*val.read().unwrap() {
                        &Value::String(ref s) => {
                            write!(handle, "{}", s);
                        }
                        _ => {
                            write!(handle, "{:?}", val);
                        }
                    }
                }
                Err(_) => {
                    write!(handle, "{:?}", val);
                }
            }

            if counter < vals_len - 1 {
                write!(handle, " ");
            }

            counter += 1;
        }

        handle.flush().unwrap();

        Ok(Value::Void.into())
    }

    fn macro_println(&mut self, args: &[Arg]) -> Result<ValueRef, Error> {
        let mut args = Vec::from(args);

        args.push(Arg {
            name: None,
            expr: Expr::Literal(Literal::String("\n".to_owned())),
        });

        self.macro_print(&args)
    }

    fn macro_readln(&mut self, args: &[Arg]) -> Result<ValueRef, Error> {
        use std::io::{self, BufRead};

        let mut vals = Vec::new();
        for arg in args {
            vals.push(self.eval_expr(&arg.expr)?);
        }

        let stdin = io::stdin();
        let mut handle = stdin.lock();

        let mut string = String::new();

        handle.read_line(&mut string).unwrap();

        Ok(Value::String(string).into())
    }

    fn macro_len(&mut self, args: &[Arg]) -> Result<ValueRef, Error> {
        let mut vals = Vec::new();
        for arg in args {
            vals.push(self.eval_expr(&arg.expr)?);
        }

        let val = match vals.get(0) {
            Some(val) => val,
            None => return Err(Error::new(ErrorKind::InvalidArgumentAmount)),
        };

        let val_read = val.read().unwrap();

        match &*val_read {
            &Value::List(List(ref val, _)) => {
                Ok(Value::Integer(val.len() as _).into())
            }
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "type doesn't implement 'len'".to_owned()))),
        }
    }

    fn create(&mut self, name: String, value: ValueRef) -> Result<(), Error> {
        self.env.write().unwrap().create(name, value)
    }

    fn assign(&mut self, name: String, value: ValueRef) -> Result<(), Error> {
        self.env.write().unwrap().assign(name, value)
    }

    fn get(&self, name: &str) -> Option<ValueRef> {
        self.env.read().unwrap().get(name)
    }
    
    fn get_from_path(&self, path: &ItemPath) -> Result<ValueRef, Error> {
        use self::mod_tree::ModuleNode;

        if path.0.len() == 0 {
            Err(Error::new(ErrorKind::InvalidPath))
        } else if path.0.len() == 1 {
            self.get(&path.0[0]).ok_or(Error::new(ErrorKind::UnknownName(path.0[0].to_owned())))
        } else {
            let mut node: &ModuleNode = &self.mod_tree;

            for i in 0..(path.0.len()-1) {
                let part = &path.0[i];

                node = node.children.get(part).ok_or(Error::new(ErrorKind::InvalidPath))?;
            }

            node.env.read().unwrap().get(&path.0[path.0.len()-1]).ok_or(Error::new(ErrorKind::InvalidPath))
        }
    }

    fn create_module(&mut self, module: &Module) -> Result<EnvironmentRef, Error> {
        let cur_env = EnvironmentRef::clone(&self.env);
        self.env = Environment::new(None).into();
        
        self.eval_module(module, false)?;

        let mod_env = EnvironmentRef::clone(&self.env);

        self.env = cur_env;

        Ok(mod_env)
    }

    pub fn create_module_tree(base: &Path) -> Result<ModuleNode, Error> {
        Self::create_module_base(base, "main")
    }

    fn create_module_base(base: &Path, name: &str) -> Result<ModuleNode, Error> {
        let mut module_path = PathBuf::from(&base);
        module_path.push(name.to_owned() + ".viper");

        let mut node = ModuleNode::new_base();
        let module = Self::load_module(&module_path)?;

        for module_name in &module.mods {

        }

        Ok(node)
    }

    fn create_module_leaf(path: &Path) -> Result<ModuleNode, Error> {
        let mut node = ModuleNode::new_leaf();
    }

    fn load_module<P: AsRef<Path>>(path: P) -> Result<Module, Error> {
        use std::fs::File;
        use std::io::Read;

        use grammar;

        let mut file = File::open(path).map_err(|_| Error::new(ErrorKind::FailedToOpenModule))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents).map_err(|_| Error::new(ErrorKind::FailedToLoadModule))?;

        match grammar::ModuleParser::new().parse(&contents) {
            Ok(module) => Ok(module),
            Err(e) => {
                panic!("{}", e);
            }
        }
    }
}