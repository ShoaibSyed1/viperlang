pub struct Block(pub Vec<Stmt>, pub Box<Expr>);

pub enum Stmt {
    Let(String, Option<Type>, Expr),
    Assign(String, Expr),
    Expr(Expr),

    Print(Expr),
}

pub enum Expr {
    UnOp(UnOp, Box<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Literal(Literal),

    Block(Block),

    If(Box<Expr>, Block, Option<Block>),
    While(Box<Expr>, Block),
    
    Cast(Type, Box<Expr>),
}

pub enum Literal {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Ident(ItemPath),
    Void,
}

pub enum UnOp {
    Neg,
    Not,
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,

    Gt,
    Gteq,
    Lt,
    Lteq,
}

pub enum Type {
    Boolean,
    Integer,
    Float,
    String,
    Custom(String), // TODO: Replace String with path type
    Void,
}

pub struct ItemPath(pub Vec<String>);