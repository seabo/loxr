use std::fmt;

use crate::chunk::Function;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Nil,
    Bool(bool),
    String(String),
    Function(Function),
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

    pub fn extract_string(&self) -> Option<String> {
        match self {
            Value::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(str) => write!(f, "{}", str),
            Value::Function(func) => write!(
                f,
                "<{}>",
                if func.name == "" {
                    String::from("<script>")
                } else {
                    String::from(&func.name)
                }
            ),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Number,
    Nil,
    Bool,
    String,
    Function,
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
        Value::Nil => Type::Nil,
        Value::Bool(_) => Type::Bool,
        Value::String(_) => Type::String,
        Value::Function(_) => Type::Function,
    }
}

pub fn is_falsey(value: &Value) -> bool {
    match value {
        Value::Nil => true,
        Value::Bool(b) => !*b,
        Value::Number(n) => *n == 0.0,
        Value::String(_) => false,
        Value::Function(_) => false,
    }
}

pub fn values_equal(value1: &Value, value2: &Value) -> bool {
    match (value1, value2) {
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::String(str1), Value::String(str2)) => str1 == str2,
        _ => false,
    }
}
