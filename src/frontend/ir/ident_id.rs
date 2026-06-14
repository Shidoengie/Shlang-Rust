use derive_more::From;
use slab::Slab;

#[derive(Debug, Clone, From)]
pub struct IdentId(usize);

#[derive(Debug, Clone, From)]
pub struct IdentTable(Slab<String>);
