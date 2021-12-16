use std::fmt::Display;

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

#[derive(Debug)]
pub struct IntObject {
    pub val: i32,
}

impl IntObject {
    pub fn new(val: i32) -> Self {
        Self { val }
    }
}

impl Display for IntObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
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
}

impl Display for BoolObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}
