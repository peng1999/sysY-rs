use std::fmt::{Display, Formatter};

use itertools::Itertools;

use super::{BranchOp, Ir, IrBlock, IrGraph, IrVec, Label, OpArg, Quaruple, Value};

impl Display for IrVec {
    // with linebreak
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for ir in &self.ir_list {
            writeln!(fmt, "{}", ir)?;
        }
        Ok(())
    }
}

impl Display for IrGraph {
    // with linebreak
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for &label in &self.block_order {
            writeln!(fmt, "{}:", label)?;
            write!(fmt, "{}", self.blocks[&label])?;
        }
        Ok(())
    }
}

impl Display for IrBlock {
    // with linebreak
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for quaruple in &self.ir_list {
            writeln!(fmt, "{}", quaruple)?;
        }
        writeln!(fmt, "{}", self.exit)
    }
}

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
            Ir::Label(label) => write!(fmt, "{}:", label),
        }
    }
}

impl Display for BranchOp {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            BranchOp::Ret(v) => write!(fmt, "ret {}", v),
            BranchOp::Goto(l) => write!(fmt, "goto {}", l),
            BranchOp::CondGoto(v, t, f) => write!(fmt, "if {} goto {} else goto {}", v, t, f),
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
            OpArg::Call { fn_val, args } => write!(fmt, "Call {} [{}]", fn_val, args.iter().join(", ")),
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
