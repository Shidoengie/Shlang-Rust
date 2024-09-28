use std::ops::{Add, Deref};
pub trait SpanUtil {
    fn get_span(&self) -> Span;
    fn take_span(self) -> Span;
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
    pub fn box_item(self) -> Spanned<Box<T>> {
        return Spanned::new(Box::new(self.item), self.span);
    }

    pub fn swap_item<U>(&self, item: U) -> Spanned<U> {
        return Spanned {
            item,
            span: self.span,
        };
    }
}
impl<T: Clone> Spanned<Box<T>> {
    pub fn deref_item<'a>(self) -> Spanned<T> {
        return Spanned::new(*self.item, self.span);
    }
}
impl<T> SpanUtil for Spanned<T> {
    fn get_span(&self) -> Span {
        self.span
    }
    fn take_span(self) -> Span {
        self.span
    }
}
pub trait IntoSpanned {
    fn to_spanned_ref(&self, span: impl SpanUtil) -> Spanned<&Self> {
        Spanned::new(self, span.take_span())
    }
    fn to_spanned(self, span: impl SpanUtil) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned::new(self, span.take_span())
    }
    fn as_spanned(&self, span: impl SpanUtil) -> Spanned<Self>
    where
        Self: Clone,
    {
        Spanned::new(self.clone(), span.take_span())
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
impl SpanUtil for Span {
    fn get_span(&self) -> Span {
        return *self;
    }
    fn take_span(self) -> Span {
        return self;
    }
}
