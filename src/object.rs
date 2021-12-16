use std::fmt::Display;

use crate::eval::EvalError;

#[derive(Debug)]
pub enum Object {
    Null,
    Int(IntObject),
    Bool(BoolObject),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(int_object) => int_object.fmt(f),
            Object::Bool(bool_object) => bool_object.fmt(f),
            Object::Null => {
                write!(f, "null")
            }
        }
    }
}

impl TryInto<BoolObject> for Object {
    type Error = EvalError;

    fn try_into(self) -> Result<BoolObject, Self::Error> {
        match self {
            Object::Null => Ok(BoolObject::new(false)),
            Object::Int(i) => {
                let b = if i.val == 0 { false } else { true };
                Ok(BoolObject::new(b))
            }
            Object::Bool(b) => Ok(b),
        }
    }
}

impl TryInto<IntObject> for Object {
    type Error = EvalError;

    fn try_into(self) -> Result<IntObject, Self::Error> {
        match self {
            Object::Int(i) => Ok(i),
            _ => Err(Self::Error::FailedToConvertIntError {
                original_type: self.to_string(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct IntObject {
    pub val: i32,
}

impl IntObject {
    pub fn new(val: i32) -> Self {
        Self { val }
    }

    pub fn negate(&self) -> Self {
        Self { val: -self.val }
    }
}

impl Display for IntObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl Into<Object> for IntObject {
    fn into(self) -> Object {
        Object::Int(self)
    }
}

#[derive(Debug)]
pub struct BoolObject {
    pub val: bool,
}

impl BoolObject {
    pub fn new(val: bool) -> Self {
        Self { val }
    }

    pub fn bang(&self) -> Self {
        Self { val: !self.val }
    }
}

impl Into<Object> for BoolObject {
    fn into(self) -> Object {
        Object::Bool(self)
    }
}

impl Display for BoolObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}
