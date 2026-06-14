use std::fmt;
use std::{
	collections::HashMap,
	hash::{BuildHasher, Hash, Hasher},
	ops::Index,
};

#[derive(Clone)]
pub struct IndexSet<T: Hash + Eq> {
	map: HashMap<u64, usize>,
	pool: Vec<T>,
	next: Vec<Option<usize>>,
}

impl<T: Hash + Eq> IndexSet<T> {
	pub fn new() -> Self {
		Self {
			map: HashMap::new(),
			pool: vec![],
			next: vec![],
		}
	}

	fn hash_item(&self, item: &T) -> u64 {
		let mut hasher = self.map.hasher().build_hasher();
		item.hash(&mut hasher);
		hasher.finish()
	}

	pub fn push(&mut self, item: T) -> usize {
		let hash = self.hash_item(&item);
		let mut curr = self.map.get(&hash).copied();

		while let Some(id) = curr {
			if self.pool[id] == item {
				return id;
			}
			curr = self.next[id];
		}

		let id = self.pool.len();
		self.pool.push(item);

		let old_head = self.map.insert(hash, id);
		self.next.push(old_head);

		id
	}

	pub fn get_id(&self, item: &T) -> Option<usize> {
		let hash = self.hash_item(item);
		let mut curr = self.map.get(&hash).copied();

		while let Some(id) = curr {
			if self.pool[id] == *item {
				return Some(id);
			}
			curr = self.next[id];
		}

		None
	}

	pub fn get(&self, id: usize) -> Option<&T> {
		self.pool.get(id)
	}
	pub fn flush(&mut self) -> Vec<T> {
		self.next.clear();
		self.map.clear();
		std::mem::take(&mut self.pool)
	}
	pub fn get_pool(self) -> Vec<T> {
		self.pool
	}
}

impl<T: Hash + Eq> Index<usize> for IndexSet<T> {
	type Output = T;

	fn index(&self, index: usize) -> &Self::Output {
		self.get(index).expect("index out of bounds")
	}
}

impl<T: Hash + Eq> Default for IndexSet<T> {
	fn default() -> Self {
		Self::new()
	}
}

impl<T: Hash + Eq + fmt::Debug> fmt::Debug for IndexSet<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_set().entries(self.pool.iter()).finish()
	}
}
