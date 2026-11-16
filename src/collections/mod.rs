pub mod charvec;
pub mod filestore;
pub mod spanmap;
pub mod spans;

use core::fmt;
use std::{collections::HashMap, hash::Hash};

pub use charvec::*;
pub use filestore::*;
pub use spanmap::*;
pub use spans::{Span, Spanned};
pub mod indexset;

pub struct SortedDebugMap<'a, T: Eq + Hash, U>(&'a HashMap<T, U>);

impl<T: Ord + Hash + Eq + fmt::Debug, U: fmt::Debug> fmt::Debug for SortedDebugMap<'_, T, U> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut entries: Vec<_> = self.0.iter().collect();
		entries.sort_unstable_by(|(left, _), (right, _)| left.cmp(right));
		f.debug_map().entries(entries).finish()
	}
}

pub fn sorted_debug_map<T: Ord + Hash + Eq, U>(map: &HashMap<T, U>) -> SortedDebugMap<'_, T, U> {
	SortedDebugMap(map)
}

#[cfg(test)]
mod tests {
	use super::sorted_debug_map;
	use std::collections::HashMap;

	#[test]
	fn sorted_debug_map_formats_entries_by_key() {
		let map = HashMap::from([(3, "three"), (1, "one"), (2, "two")]);
		assert_eq!(
			format!("{:?}", sorted_debug_map(&map)),
			"{1: \"one\", 2: \"two\", 3: \"three\"}"
		);
	}
}
