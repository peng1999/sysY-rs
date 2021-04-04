use std::fmt::{Display, Formatter};

use super::{OpArg, Quaruple, Reg, Value};

impl Display for Quaruple {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(r) = self.result {
            write!(fmt, "{} = ", r)?;
        }
        write!(fmt, "{}", self.op)?;
        Ok(())
    }
}

impl Display for Reg {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.is_const {
            write!(fmt, "%")?;
        } else {
            write!(fmt, "@")?;
        }
        write!(fmt, "{}", self.sym.0)?;
        Ok(())
    }
}

impl Display for OpArg {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            OpArg::Unary { op, arg } => write!(fmt, "{:?} {}", op, arg)?,
            OpArg::Binary { op, arg1, arg2 } => write!(fmt, "{:?} {}, {}", op, arg1, arg2)?,
        }
        Ok(())
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Reg(r) => write!(fmt, "{}", r)?,
            Value::Int(i) => write!(fmt, "{}", i)?,
        }
        Ok(())
    }
}
