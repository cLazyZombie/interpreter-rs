use std::fmt::Display;

use crate::eval::EvalError;

#[derive(Debug)]
pub enum Object {
    Null,
    Int(IntObject),
    Bool(BoolObject),
}

impl Object {
    pub fn plus(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.plus(rhs).map(|i| i.into()),
            _ => None,
        }
    }

    pub fn minus(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.minus(rhs).map(|i| i.into()),
            _ => None,
        }
    }

    pub fn asterrisk(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.asterrisk(rhs).map(|i| i.into()),
            _ => None,
        }
    }

    pub fn slash(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.slash(rhs).map(|i| i.into()),
            _ => None,
        }
    }

    pub fn eq(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.eq(rhs).map(|i| i.into()),
            Object::Bool(bool_object) => bool_object.eq(rhs).map(|b| b.into()),
            _ => None,
        }
    }

    pub fn not_eq(&self, rhs: Self) -> Option<Object> {
        match self {
            Object::Int(int_object) => int_object.not_eq(rhs).map(|i| i.into()),
            Object::Bool(bool_object) => bool_object.not_eq(rhs).map(|b| b.into()),
            _ => None,
        }
    }
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
                let b = i.val != 0;
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

    pub fn plus(&self, rhs: Object) -> Option<IntObject> {
        let rhs: Result<IntObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(IntObject::new(self.val + rhs.val))
        } else {
            None
        }
    }

    pub fn minus(&self, rhs: Object) -> Option<IntObject> {
        let rhs: Result<IntObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(IntObject::new(self.val - rhs.val))
        } else {
            None
        }
    }

    pub fn asterrisk(&self, rhs: Object) -> Option<IntObject> {
        let rhs: Result<IntObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(IntObject::new(self.val * rhs.val))
        } else {
            None
        }
    }

    pub fn slash(&self, rhs: Object) -> Option<IntObject> {
        let rhs: Result<IntObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(IntObject::new(self.val / rhs.val))
        } else {
            None
        }
    }

    pub fn eq(&self, rhs: Object) -> Option<BoolObject> {
        let rhs: Result<IntObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(BoolObject::new(self.val == rhs.val))
        } else {
            None
        }
    }

    pub fn not_eq(&self, rhs: Object) -> Option<BoolObject> {
        self.eq(rhs).map(|b| BoolObject::new(!b.val))
    }
}

impl Display for IntObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<IntObject> for Object {
    fn from(i: IntObject) -> Self {
        Object::Int(i)
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

    pub fn eq(&self, rhs: Object) -> Option<BoolObject> {
        let rhs: Result<BoolObject, EvalError> = rhs.try_into();
        if let Ok(rhs) = rhs {
            Some(BoolObject::new(self.val == rhs.val))
        } else {
            None
        }
    }

    pub fn not_eq(&self, rhs: Object) -> Option<BoolObject> {
        self.eq(rhs).map(|b| BoolObject::new(!b.val))
    }
}

impl From<BoolObject> for Object {
    fn from(b: BoolObject) -> Self {
        Object::Bool(b)
    }
}

impl Display for BoolObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}
