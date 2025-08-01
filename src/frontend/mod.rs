pub mod ast;
pub mod ir;
mod lexemes;
pub mod nameres;
pub use ast::Parser;
pub use ir::IRgen;
pub use lexemes::lexer::Lexer;
pub use lexemes::tokens;

pub mod errors {
    pub use super::ast::error::ParseError;
    pub use super::nameres::error::NameErr;
}
