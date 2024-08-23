use std::ops::Add;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Span(pub usize, pub usize);
impl Add<Self> for Span {
    type Output = Span;
    fn add(self, rhs: Span) -> Self::Output {
        Span(self.0, rhs.1)
    }
}
impl Add<usize> for Span {
    type Output = Span;
    fn add(self, rhs: usize) -> Self::Output {
        Span(self.0, self.1 + rhs)
    }
}
