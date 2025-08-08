use crate::{lang_errors::LangError, spans::Spanned};
#[derive(Debug)]
pub enum NameErr {
    UndefinedVar(String),
    Unspecified(String),
}
impl LangError for Spanned<NameErr> {
    fn msg(&self) -> String {
        match &self.item {
            NameErr::UndefinedVar(var) => {
                format!("Undefined variable {var}")
            }
            NameErr::Unspecified(msg) => msg.to_string(),
        }
    }
}
