use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::eval::EvalError;

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Int(IntObject),
    Bool(BoolObject),
    Return(ReturnObject),
}

impl Object {
    pub fn plus(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs + rhs).into()),
            _ => None,
        }
    }

    pub fn minus(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs - rhs).into()),
            _ => None,
        }
    }

    pub fn asterrisk(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs * rhs).into()),
            _ => None,
        }
    }

    pub fn slash(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs / rhs).into()),
            _ => None,
        }
    }

    pub fn eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs == rhs))),
            (Object::Bool(lhs), Object::Bool(rhs)) => {
                Some(Object::Bool(BoolObject::new(lhs == rhs)))
            }
            _ => None,
        }
    }

    pub fn not_eq(&self, rhs: &Self) -> Option<Object> {
        self.eq(rhs).map(|b| {
            if let Object::Bool(b) = b {
                b.bang().into()
            } else {
                panic!("equal should return bool object");
            }
        })
    }

    pub fn lt(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs < rhs))),
            _ => None,
        }
    }

    pub fn lt_eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs <= rhs))),
            _ => None,
        }
    }

    pub fn gt(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs > rhs))),
            _ => None,
        }
    }

    pub fn gt_eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs >= rhs))),
            _ => None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(int_object) => int_object.fmt(f),
            Object::Bool(bool_object) => bool_object.fmt(f),
            Object::Return(return_object) => return_object.fmt(f),
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
            Object::Return(ret) => {
                let val = ret.val;
                let obj = *val;
                obj.try_into()
            }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

impl Add for &IntObject {
    type Output = IntObject;

    fn add(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val + rhs.val)
    }
}

impl Sub for &IntObject {
    type Output = IntObject;

    fn sub(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val - rhs.val)
    }
}

impl Mul for &IntObject {
    type Output = IntObject;

    fn mul(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val * rhs.val)
    }
}

impl Div for &IntObject {
    type Output = IntObject;

    fn div(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val / rhs.val)
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct ReturnObject {
    pub val: Box<Object>,
}

impl ReturnObject {
    pub fn new(val: Object) -> Self {
        Self { val: Box::new(val) }
    }
}

impl Display for ReturnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &*self.val)
    }
}

impl From<ReturnObject> for Object {
    fn from(ret: ReturnObject) -> Self {
        Object::Return(ret)
    }
}
