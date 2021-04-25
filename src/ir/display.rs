use std::fmt::{Display, Formatter};

use super::{BranchOp, Ir, Label, OpArg, Quaruple, Value};

impl Display for Label {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "L{}", self.0)
    }
}

impl Display for Ir {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Ir::Quaruple(quaruple) => write!(fmt, "{}", quaruple),
            Ir::Branch(branch) => write!(fmt, "{}", branch),
        }
    }
}

impl Display for BranchOp {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BranchOp::Ret(v) => write!(fmt, "ret {}", v),
            BranchOp::Goto(l) => write!(fmt, "goto {}", l),
            BranchOp::CondGoto(v, t, f) => write!(fmt, "if {} goto {} else goto{}", v, t, f),
        }
    }
}

impl Display for Quaruple {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(r) = self.result {
            write!(fmt, "%{} = ", r.0)?;
        }
        write!(fmt, "{}", self.op)
    }
}

impl Display for OpArg {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OpArg::Unary { op, arg } => write!(fmt, "{:?} {}", op, arg),
            OpArg::Binary { op, arg1, arg2 } => write!(fmt, "{:?} {}, {}", op, arg1, arg2),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Reg(r) => write!(fmt, "%{}", r.0),
            Value::Int(i) => write!(fmt, "{}", i),
            Value::Bool(b) => write!(fmt, "{}", b),
        }
    }
}
