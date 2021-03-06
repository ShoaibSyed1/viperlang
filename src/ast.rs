pub struct Module {
    pub classes: Vec<Class>,
    pub functions: Vec<Function>,
    pub mods: Vec<Mod>,
}

pub type Mod = String;

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub static_fields: Vec<Field>,
    pub fields: Vec<Field>,
    pub functions: Vec<Function>,
    pub methods: Vec<Method>,
}

#[derive(Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone)]
pub struct Method(pub Function);

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Expr,
}

#[derive(Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub default: Option<Literal>,
}

#[derive(Clone)]
pub struct Block(pub Vec<Stmt>, pub Box<Expr>);

#[derive(Clone)]
pub enum Stmt {
    Let(String, Option<Type>, Expr),
    Assign(Expr, Expr),
    Expr(Expr),

    Return(Expr),
    Break(Expr),
}

#[derive(Clone)]
pub enum Expr {
    UnOp(UnOp, Box<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Literal(Literal),

    Block(Block),

    If(Box<Expr>, Block, Option<Block>),
    IfHas(Box<Expr>, String, Block, Option<Block>),
    While(Box<Expr>, Block),
    For(String, Box<Expr>, Block),

    Call(Box<Expr>, Vec<Arg>),

    Make(Box<Expr>, Vec<MakeArg>),
    MakeList(Type, ListInit),

    Dot(Box<Expr>, String),
    
    Cast(Type, Box<Expr>),

    OptionSome(Box<Expr>),

    Macro(String, Vec<Arg>),

    Index(Box<Expr>, Box<Expr>),
}

#[derive(Clone)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Ident(ItemPath),
    Void,
    None(Type),
}

#[derive(Clone)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Modulo,

    Eq,
    Neq,

    Gt,
    Gteq,
    Lt,
    Lteq,

    Or,
    And,
}

#[derive(Clone)]
pub enum ListInit {
    Items(Vec<Expr>),
    Duplicate(Box<Expr>, u32),
}

#[derive(Clone)]
pub struct Arg {
    pub name: Option<String>,
    pub expr: Expr,
}

#[derive(Clone)]
pub struct MakeArg {
    pub name: String,
    pub expr: Expr,
}

#[derive(Clone)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    String,
    Class(String), // TODO: Replace String with path type
    Void,
    Option(Box<Type>),
    List(Box<Type>),
}

#[derive(Clone)]
pub struct ItemPath(pub Vec<String>);