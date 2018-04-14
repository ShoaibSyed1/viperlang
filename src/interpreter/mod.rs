use ast::{BinOp, Block, Expr, Literal, Stmt, Type as AstType, UnOp};

mod environment;
mod error;
mod value;

use self::environment::Environment;
use self::error::{Error, ErrorKind};
use self::value::{Type, Value, ValueRc};

pub struct Interpreter {
    env: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Box::new(Environment::new(None)),
        }
    }

    pub fn eval_block(&mut self, block: &Block) -> Result<ValueRc, Error> {
        let env = Environment::new(Some(::std::mem::replace(&mut self.env, Box::new(Environment::new(None)))));
        self.env = Box::new(env);

        for stmt in &block.0 {
            self.eval_stmt(stmt)?;
        }

        let ret = self.eval_expr(&block.1)?;

        self.env = self.env.get_parent().unwrap_or_else(|| Box::new(Environment::new(None)));
        
        Ok(ret)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            &Stmt::Let(ref name, ref opt_type, ref expr) => {
                let val = self.eval_expr(expr)?;

                if let Some(ref ty) = opt_type {
                    let ty = self.ast_type_to_runtime_type(ty)?;

                    if ty != val.get_type() {
                        return Err(Error::new(ErrorKind::TypeMismatch(ty, val.get_type(), "cannot assign 'type2' to 'type1' variable".into())));
                    }
                }

                self.env.create(name.to_owned(), val)
            }
            &Stmt::Assign(ref name, ref expr) => {
                let val = self.eval_expr(expr)?;

                self.env.assign(name.to_owned(), val)
            }
            &Stmt::Expr(ref expr) => {
                self.eval_expr(expr)?;

                Ok(())
            }

            &Stmt::Print(ref expr) => {
                let val = self.eval_expr(expr)?;

                let val = match self.do_cast(&Type::String, &val) {
                    Ok(val) => val,
                    Err(_) => {
                        println!("{:?}", val);
                        return Ok(());
                    }
                };

                match &*val {
                    &Value::String(ref s) => println!("{}", s),
                    _ => {
                        println!("{:?}", val);
                    }
                }

                Ok(())
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<ValueRc, Error> {
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

            &Expr::Cast(ref ast_type, ref expr) => {
                let val = self.eval_expr(expr)?;
                let ty = self.ast_type_to_runtime_type(ast_type)?;

                self.do_cast(&ty, &val)
            }

            &Expr::If(ref cond, ref block, ref else_block) => self.eval_if(cond, block, else_block),
            &Expr::While(ref cond, ref block) => self.eval_while(cond, block),
        }
    }

    fn eval_if(&mut self, cond: &Expr, block: &Block, else_block: &Option<Block>) -> Result<ValueRc, Error> {
        let cond = self.eval_expr(cond)?;
        let cond = match &*cond {
            &Value::Boolean(b) => b,
            other => return Err(Error::new(ErrorKind::TypeMismatch(Type::Boolean, other.get_type(), "expected type 'bool' in if condition".to_owned()))),
        };

        if cond {
            self.eval_block(block)
        } else {
            if let Some(ref block) = else_block.as_ref() {
                self.eval_block(block)
            } else {
                Ok(Value::Void.into())
            }
        }
    }

    fn eval_while(&mut self, cond: &Expr, block: &Block) -> Result<ValueRc, Error> {
        loop {
            let cond = self.eval_expr(cond)?;
            let cond = match &*cond {
                &Value::Boolean(b) => b,
                other => return Err(Error::new(ErrorKind::TypeMismatch(Type::Boolean, other.get_type(), "expected type 'bool' in while condition".to_owned()))),
            };

            if cond {
                self.eval_block(block)?;
            } else {
                break;
            }
        }

        Ok(Value::Void.into())
    }

    fn eval_literal(&mut self, literal: &Literal) -> Result<ValueRc, Error> {
        match literal {
            &Literal::Boolean(val) => Ok(Value::Boolean(val).into()),
            &Literal::Integer(val) => Ok(Value::Integer(val).into()),
            &Literal::Float(val) => Ok(Value::Float(val).into()),
            &Literal::String(ref val) => Ok(Value::String(val.to_owned()).into()),
            &Literal::Ident(ref item_path) => {
                // TODO: Proper path resolution
                self.env.get(&item_path.0[0]).ok_or(Error::new(ErrorKind::UnknownName(item_path.0[0].to_owned())))
            }
            &Literal::Void => Ok(Value::Void.into()),
        }
    }

    fn do_unop(&self, op: &UnOp, val: &Value) -> Result<ValueRc, Error> {
        match op {
            &UnOp::Neg => self.do_neg(val),
            &UnOp::Not => self.do_not(val),
        }
    }

    fn do_neg(&self, val: &Value) -> Result<ValueRc, Error> {
        match val {
            &Value::Integer(i) => Ok(Value::Integer(-i).into()),
            &Value::Float(f) => Ok(Value::Float(-f).into()),
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "no implementation of 'neg' for this type".to_owned()))),
        }
    }

    fn do_not(&self, val: &Value) -> Result<ValueRc, Error> {
        match val {
            &Value::Boolean(b) => Ok(Value::Boolean(!b).into()),
            val => Err(Error::new(ErrorKind::TypeMismatch(val.get_type(), val.get_type(), "no implementation of 'not' for this type".to_owned()))),
        }
    }

    fn do_binop(&self, l: &Value, op: &BinOp, r: &Value) -> Result<ValueRc, Error> {
        match op {
            &BinOp::Add => self.do_add(l, r),
            &BinOp::Sub => self.do_sub(l, r),
            &BinOp::Mul => self.do_mul(l, r),
            &BinOp::Div => self.do_div(l, r),

            &BinOp::Eq => self.do_eq(l, r),
            &BinOp::Neq => self.do_neq(l, r),
            
            &BinOp::Gt => self.do_gt(l, r),
            &BinOp::Gteq => self.do_gteq(l, r),
            &BinOp::Lt => self.do_lt(l, r),
            &BinOp::Lteq => self.do_lteq(l, r),
        }
    }

    fn do_add(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 + i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 + f2).into()),
            (&Value::String(ref s1), &Value::String(ref s2)) => Ok(Value::String(s1.to_owned() + s2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'add' for these types".to_owned()))),
        }
    }

    fn do_sub(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 - i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 - f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'sub' for these types".to_owned()))),
        }
    }

    fn do_mul(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 * i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 * f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'mul' for these types".to_owned()))),
        }
    }

    fn do_div(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Integer(i1 / i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Float(f1 / f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'div' for these types".to_owned()))),
        }
    }

    fn do_eq(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Boolean(b1), &Value::Boolean(b2)) => Ok(Value::Boolean(b1 == b2).into()),
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 == i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 == f2).into()),
            (&Value::String(ref s1), &Value::String(ref s2)) => Ok(Value::Boolean(s1 == s2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'eq' for these types".to_owned()))),
        }
    }

    fn do_neq(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        self.do_eq(val1, val2).map(|v| if let &Value::Boolean(b) = &*v {
            ValueRc::new(Value::Boolean(!b))
        } else { v.into() })
    }

    fn do_gt(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 > i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 > f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'gt' for these types".to_owned()))),
        }
    }

    fn do_gteq(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 >= i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 >= f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'gteq' for these types".to_owned()))),
        }
    }

    fn do_lt(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 < i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 < f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'lt' for these types".to_owned()))),
        }
    }

    fn do_lteq(&self, val1: &Value, val2: &Value) -> Result<ValueRc, Error> {
        match (val1, val2) {
            (&Value::Integer(i1), &Value::Integer(i2)) => Ok(Value::Boolean(i1 <= i2).into()),
            (&Value::Float(f1), &Value::Float(f2)) => Ok(Value::Boolean(f1 <= f2).into()),
            (a, b) => Err(Error::new(ErrorKind::TypeMismatch(a.get_type(), b.get_type(), "no implementation of 'lteq' for these types".to_owned()))),
        }
    }

    fn do_cast(&self, ty: &Type, val: &Value) -> Result<ValueRc, Error> {
        use std::str::FromStr;

        if &val.get_type() == ty { return Ok(val.clone().into()); }

        match (ty, val) {
            (&Type::Float, &Value::Integer(i)) => Ok(Value::Float(i as f64).into()),
            (&Type::Float, &Value::String(ref s)) => Ok(Value::Float(match f64::from_str(s) {
                Ok(f) => f,
                Err(_) => return Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val.clone(), "invalid value".to_owned()))),
            }).into()),

            (&Type::Integer, &Value::Float(f)) => Ok(Value::Integer(f as i64).into()),
            (&Type::Integer, &Value::String(ref s)) => Ok(Value::Integer(match i64::from_str(s) {
                Ok(i) => i,
                Err(_) => return Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val.clone(), "invalid value".to_owned()))),
            }).into()),

            (&Type::String, &Value::Integer(i)) => Ok(Value::String(i.to_string()).into()),
            (&Type::String, &Value::Float(f)) => Ok(Value::String(f.to_string()).into()),
            (&Type::String, &Value::Boolean(b)) => Ok(Value::String(b.to_string()).into()),
            (&Type::String, &Value::Void) => Ok(Value::String("void".to_owned()).into()),

            (ty, val) => Err(Error::new(ErrorKind::InvalidCast(ty.clone(), val.clone(), "cast not supported between these types".to_owned()))),
        }
    }

    fn ast_type_to_runtime_type(&self, ast_type: &AstType) -> Result<Type, Error> {
        match ast_type {
            &AstType::Boolean => Ok(Type::Boolean),
            &AstType::Integer => Ok(Type::Integer),
            &AstType::Float => Ok(Type::Float),
            &AstType::String => Ok(Type::String),
            &AstType::Custom(_) => unimplemented!(),
            &AstType::Void => Ok(Type::Void),
        }
    }
}