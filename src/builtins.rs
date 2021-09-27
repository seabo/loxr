use std::time::{SystemTime, UNIX_EPOCH};

use crate::value;
use crate::vm;

pub fn clock(_vm: &mut vm::VM, _args: &[value::Value]) -> Result<value::Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Ok(value::Value::Number(since_the_epoch.as_millis() as f64))
}
