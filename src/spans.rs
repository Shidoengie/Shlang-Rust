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
pub type Span = (usize, usize);
