pub struct Quaruple {
    op: OpCode,
    result: String,
    arg1: Option<String>,
    arg2: Option<String>,
}

pub enum OpCode {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}
