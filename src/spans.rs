#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub unspanned: T,
    pub span: Span
}
pub type Span = (usize, usize);
