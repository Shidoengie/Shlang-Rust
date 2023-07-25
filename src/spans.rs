#[derive( Debug,Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub unspanned: T,
    pub span: Span
}
impl<T> Spanned<T>{
    pub fn new(unspanned: T,span:Span)->Self{
        Self {unspanned, span}
    }
}
// impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T>{
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.debug_struct(format!("{:#?}",self.unspanned).as_str()).field("span", &self.span).finish()
//     }
// }
pub type Span = (usize, usize);
