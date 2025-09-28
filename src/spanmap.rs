use std::{fmt::Debug, ops::Index};

use crate::spans::{IntoSpanned, Span, Spanned};

#[derive(Clone, Default)]
pub struct SpanMap {
    /// The map is a vector of `((start, stop), Span)`.
    /// **NO Invariants**:  Ranges can overlap and the vector is unsorted.
    map: Vec<Spanned<(usize, usize)>>,
}
impl Debug for SpanMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SpanMap {{")?;
        if !self.map.is_empty() {
            writeln!(f)?;
        }
        for entry in self.map.iter() {
            let Spanned { item, span } = entry;
            writeln!(
                f,
                "    {start_index}..{stop_index} = Span[{start_range},{end_range}],",
                start_index = item.0,
                stop_index = item.1,
                start_range = span.start,
                end_range = span.end
            )?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}
impl SpanMap {
    /// Creates a new empty span maps
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a new span.  No checks are performed.
    /// Ranges are half-open: `[start, stop)`.
    pub fn push(&mut self, start: usize, stop: usize, span: Span) {
        self.map.push((start, stop).to_spanned(span));
    }

    /// Checks if the given range `[start, stop)` overlaps with any existing span.
    pub fn has_op_range(&self, start: usize, stop: usize) -> bool {
        if start >= stop {
            return false; // Invalid range, no overlap possible
        }

        for Spanned {
            item: (r_start, r_stop),
            span: _,
        } in &self.map
        {
            if *r_start < stop && start < *r_stop {
                return true;
            }
        }

        false
    }

    /// Gets a reference to the span containing a given `op_index`. Returns the most specific match (smallest range containing index).
    /// Ranges are treated as half-open: `[start, stop)`.
    pub fn get(&self, op_index: usize) -> Option<&Span> {
        let mut best_span: Option<&Span> = None;
        let mut best_len: Option<usize> = None;

        for Spanned {
            item: (start, stop),
            span,
        } in &self.map
        {
            if op_index >= *start && op_index < *stop {
                let len = stop - start;

                match best_len {
                    Some(best_length) => {
                        if len < best_length {
                            best_span = Some(span);
                            best_len = Some(len);
                        }
                    }
                    None => {
                        // If no best span exists, this is the first match, so it's the best for now
                        best_span = Some(span);
                        best_len = Some(len);
                    }
                }
            }
        }

        best_span
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
