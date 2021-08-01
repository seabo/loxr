use std::fmt;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
}

#[allow(dead_code)]
impl Value {
    pub fn print(&self) {
        println!("{}", self);
    }

    pub fn extract_number(&self) -> Option<f64> {
        match self {
            Value::Number(f) => Some(*f),
            _ => None,
        }
    }

    pub fn extract_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Bool(bool) => write!(f, "{}", bool),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Number,
    Nil,
    Bool,
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
        Value::Nil => Type::Nil,
        Value::Bool(_) => Type::Bool,
    }
}

pub fn is_falsey(value: &Value) -> bool {
    match value {
        Value::Nil => true,
        Value::Bool(b) => *b,
        Value::Number(n) => *n == 0.0,
    }
}

pub fn values_equal(value1: &Value, value2: &Value) -> bool {
    match (value1, value2) {
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        _ => false,
    }
}
