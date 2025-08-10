use std::fmt::Display;

pub enum ErrCode {
    Unspecified(String),
}
pub struct VmErr {
    index: usize,
    code: ErrCode,
}
impl VmErr {
    pub fn new(index: usize, code: ErrCode) -> Self {
        return Self { index, code };
    }
    pub fn other(index: usize, msg: impl Display) -> Self {
        return Self {
            index,
            code: ErrCode::Unspecified(msg.to_string()),
        };
    }
}
