/// A run-length encoded vector for storing any type
/// This is more memory efficient when there are many consecutive identical values
#[derive(Clone, Debug)]
pub struct RleVec<T> {
    /// The values of each run
    values: Vec<T>,
    /// The number of items in each run
    counts: Vec<usize>,
}

impl<T: Clone + PartialEq> RleVec<T> {
    /// Creates a new empty RleVec
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            counts: Vec::new(),
        }
    }

    /// Pushes a new value to the vector
    pub fn push(&mut self, value: T) {
        // If the last run has the same value, just increment its count
        if !self.values.is_empty() && self.values.last().unwrap() == &value {
            *self.counts.last_mut().unwrap() += 1;
            return;
        }

        // Otherwise, start a new run
        self.values.push(value);
        self.counts.push(1);
    }

    /// Gets the value at the given index
    pub fn get(&self, index: usize) -> Option<&T> {
        let mut remaining = index;
        for i in 0..self.values.len() {
            if remaining < self.counts[i] {
                return Some(&self.values[i]);
            }
            remaining -= self.counts[i];
        }
        None
    }

    /// Returns the number of items in the vector
    pub fn len(&self) -> usize {
        self.counts.iter().sum()
    }

    /// Returns true if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Clears the vector
    pub fn clear(&mut self) {
        self.values.clear();
        self.counts.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(vec![1], vec![1], vec![1])]
    #[case(vec![1, 2, 3], vec![1, 2, 3], vec![1, 1, 1])]
    #[case(vec![1, 1, 1], vec![1], vec![3])]
    #[case(vec![1, 1, 1, 2, 3, 3], vec![1, 2, 3], vec![3, 1, 2])]
    #[case(vec![1, 2, 2, 2, 1], vec![1, 2, 1], vec![1, 3, 1])]
    fn test_rle_vec(
        #[case] values: Vec<usize>,
        #[case] expected_values: Vec<usize>,
        #[case] expected_counts: Vec<usize>,
    ) {
        let mut vec = RleVec::new();
        for &value in &values {
            vec.push(value);
        }

        assert_eq!(vec.len(), values.len());
        for i in 0..values.len() {
            assert_eq!(vec.get(i), Some(&values[i]));
        }

        assert_eq!(vec.values, expected_values);
        assert_eq!(vec.counts, expected_counts);
    }
}
