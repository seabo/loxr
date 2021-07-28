pub type Value = f64;

#[derive(Debug, Default, Clone)]
pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn write_value_array(&mut self, value: Value) {
        self.values.push(value);
    }
}
