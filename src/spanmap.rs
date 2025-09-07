use std::{cmp::Ordering, ops::Index};

use crate::spans::Span;

#[derive(Clone, Debug, Default)]
pub struct SpanMap {
    /// The map is a vector of `((start, stop), Span)`.
    /// **Invariant**: To enable binary search, this vector must be kept
    /// sorted by `start` index, and its ranges must not overlap.
    map: Vec<((usize, usize), Span)>,
}

impl SpanMap {
    /// Creates a new empty span maps
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a new span, ensuring it maintains the sorted, non-overlapping invariant.
    /// Ranges are half-open: `[start, stop)`.
    pub fn push(&mut self, start: usize, stop: usize, span: Span) -> Result<(), ()> {
        let is_invalid_range = start >= stop;

        if is_invalid_range {
            return Err(());
        }

        if let Some(((_last_start, last_stop), _)) = self.map.last() {
            // This check maintains both sorted order and prevents overlaps.
            let violates_invariant = start < *last_stop;
            if violates_invariant {
                return Err(());
            }
        }

        self.map.push(((start, stop), span));
        Ok(())
    }

    /// Adds a new span without any checks. This is faster but can break invariants.
    /// **Use only if you can guarantee ranges are sorted and non-overlapping.**
    pub fn push_unchecked(&mut self, start: usize, stop: usize, span: Span) {
        self.map.push(((start, stop), span));
    }

    /// Checks if the given range `[start, stop)` overlaps with any existing span.
    pub fn has_op_range(&self, start: usize, stop: usize) -> bool {
        let is_empty_or_invalid = self.map.is_empty() || start >= stop;
        if is_empty_or_invalid {
            return false;
        }

        let first_possible_idx = match self.map.binary_search_by_key(&start, |((s, _), _)| *s) {
            Ok(idx) => idx,
            Err(idx) => idx,
        };

        // An overlap is only possible with the range immediately preceding the
        // search result or the one at the result index itself.
        if let Some(prev_idx) = first_possible_idx.checked_sub(1) {
            if let Some(((r_start, r_stop), _)) = self.map.get(prev_idx) {
                let has_overlap = *r_start < stop && start < *r_stop;
                if has_overlap {
                    return true;
                }
            }
        }

        if let Some(((r_start, r_stop), _)) = self.map.get(first_possible_idx) {
            let has_overlap = *r_start < stop && start < *r_stop;
            if has_overlap {
                return true;
            }
        }

        false
    }

    /// Gets a reference to the span containing a given `op_index` using binary search.
    /// Ranges are treated as half-open: `[start, stop)`.
    pub fn get(&self, op_index: usize) -> Option<&Span> {
        let search_result = self.map.binary_search_by(|((start, stop), _span)| {
            if op_index < *start {
                Ordering::Greater
            } else if op_index >= *stop {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        search_result.ok().map(|index| &self.map[index].1)
    }
}

/// Allows `span_map[op_index]` access.
///
/// # Panics
/// Panics if `op_index` is not contained within any span range.
impl Index<usize> for SpanMap {
    type Output = Span;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
            .expect("Index out of bounds of any span range")
    }
}
