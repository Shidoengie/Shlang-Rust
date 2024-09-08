use rayon::prelude::*;
use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
};
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CharVec(pub Vec<char>);
impl Display for CharVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_par_iter(self.0.par_iter()))
    }
}
impl Debug for CharVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", String::from_par_iter(self.0.par_iter()))
    }
}
impl From<String> for CharVec {
    fn from(value: String) -> Self {
        let val: Vec<char> = value.par_chars().collect();
        return Self(val);
    }
}
impl From<&str> for CharVec {
    fn from(value: &str) -> Self {
        let val: Vec<char> = value.par_chars().collect();
        return Self(val);
    }
}
impl Index<usize> for CharVec {
    type Output = char;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl IndexMut<usize> for CharVec {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
