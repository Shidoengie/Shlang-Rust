use std::ops::Add;
pub trait SpanUtil {
    fn get_span(&self) -> Span;
}

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
impl<T> SpanUtil for Spanned<T> {
    fn get_span(&self) -> Span {
        self.span
    }
}
pub trait IntoSpanned {
    fn to_spanned_ref(&self, span: Span) -> Spanned<&Self> {
        Spanned::new(self, span)
    }
    fn to_spanned(&self, span: Span) -> Spanned<Self>
    where
        Self: Clone,
    {
        Spanned::new(self.clone(), span)
    }
}
impl<T> IntoSpanned for T {}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Span(pub usize, pub usize);
impl Span {
    pub const EMPTY: Self = Self(0, 0);
}
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
