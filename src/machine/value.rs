use std::{
    cmp::Ordering, fmt::Display, hash::{Hash, Hasher}, ops::{Add, Div, Mul, Neg, Not, Sub}
};

use crate::error::RuntimeError;

#[derive(Debug)]
pub enum Value {
    Float(f64),
    Int(i64),
    Bool(bool),
    String(String),   
}

impl Clone for Value {
     fn clone(&self) -> Self {
         match self {
             Value::Float(a) => Value::Float(*a),
             Value::Int(a) => Value::Int(*a),
             Value::Bool(a) => Value::Bool(*a),
             Value::String(a) => Value::String(a.clone()),
         }
     }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(a) => write!(f, "{}", a),
            Value::Int(a) => write!(f, "{}", a),
            Value::Bool(a) => write!(f, "{}", a),
            Value::String(a) => write!(f, "{}", a),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, RuntimeError>;

    fn add(self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
            (Value::String(a), Value::Int(b)) => Ok(Value::String(a + &b.to_string())),
            (Value::Int(a), Value::String(b)) => Ok(Value::String(a.to_string() + &b)),
            (Value::String(a), Value::Float(b)) => Ok(Value::String(a + &b.to_string())),
            (Value::Float(a), Value::String(b)) => Ok(Value::String(a.to_string() + &b)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "+".to_string()
            }),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, RuntimeError>;

    fn sub(self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "-".to_string()
            }),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, RuntimeError>;

    fn mul(self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "*".to_string()
            }),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, RuntimeError>;

    fn div(self, other: Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / b as f64)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 / b)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "/".to_string()
            }),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, RuntimeError>;

    fn neg(self) -> Result<Value, RuntimeError> {
        match self {
            Value::Float(a) => Ok(Value::Float(-a)),
            Value::Int(a) => Ok(Value::Int(-a)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "-".to_string()
            }),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, RuntimeError>;

    fn not(self) -> Result<Value, RuntimeError> {
        match self {
            Value::Bool(a) => Ok(Value::Bool(!a)),
            _ => Err(RuntimeError::InvalidOperation {
                operation: "!".to_string()
            }),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(&b),
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(&b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(&b),
            _ => None,
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(a) => {
                // TODO: Support hashable NaN and Infinity
                // TODO: Do we want NaN and Infinity to be possible values in the language?
                if a.is_nan() {
                    f64::NAN.to_bits().hash(state);
                } else if a.is_infinite() {
                    f64::INFINITY.to_bits().hash(state);
                } else {
                    a.to_bits().hash(state);
                }
            },
            Value::Int(a) => a.hash(state),
            Value::Bool(a) => a.hash(state),
            Value::String(a) => a.hash(state),
        }
    }
}


