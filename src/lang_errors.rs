use crate::spans::*;
use colored::*;
#[derive(Clone)]
pub struct LangError {
    pub input: String,
}

impl LangError {
    
    pub fn emit(&mut self, msg: &str, span: Span){
        println!("{}",self.build(msg, span));
    }
    pub fn panic_emit(&mut self, msg: &str, span: Span){
        self.emit(msg, span);
        panic!()
    }
    pub fn build(&mut self, msg: &str, span: Span) -> String {
        let position = self.line_pos(span.clone());
        let (start, stop) = (
            self.input[..span.0].to_string(),
            self.input[span.1..].to_string(),
        );
        let marked = format!(
            "{start}{}{stop}",
            self.input[span.0..span.1].to_string().red()
        );
        let lines: Vec<&str> = marked.lines().collect();
        let line = lines[position - 1];
        let I = "|".blue();
        return format!("{} {msg}\n{position} {I} {line}", "ERROR!".red(),);
    }
    pub fn line_pos(&mut self, span: Span) -> usize {
        return self.input[..span.0]
            .chars()
            .filter(|ch| *ch == '\n')
            .count()
            + 1;
    }
}
