use std::collections::HashSet;
use crate::bit_array::BitArray;

pub struct BinaryPrefixSet {
	prefixes_map: HashSet<String>,
	max_width: u32,
}



impl BinaryPrefixSet {
	pub fn new(max_width: u32) -> Self {
		BinaryPrefixSet{ prefixes_map:HashSet::new(), max_width }
	}

	pub fn add(&mut self, number: u32) {
		let bA = BitArray::new(number, self.max_width);
		let bitStr = bA.to_string();
		for i in 1..self.max_width+1 {
			let bi = String::from(&bitStr.as_str()[..i as usize]);
			self.prefixes_map.insert( bi );
		}
	}


	// return max xor possible with the current prefixes map
	// and the given number
	pub fn calculate_max_xor(&self, number: u32) -> u32 {
		let flip = |x| if x==1 {0} else {1};
		let mut current_xor = BitArray::new(0, self.max_width);

		let mut current_prefix = String::from("");
		let bA = BitArray::new(number, self.max_width);
		for i in 0..self.max_width {
			let b: u32 = bA.get(i).unwrap();
			let b_:u32 = flip(b);

			if self.prefixes_map.contains( &String::from( current_prefix.clone() + &b_.to_string() )) {
				current_xor.set(i, 1);
				current_prefix = String::from(current_prefix + &b_.to_string());
			} else {
				current_xor.set(i, 0); // this is redundant, but for the sake of consistency
				current_prefix = String::from(current_prefix + &b.to_string());
			}
		}
		current_xor.number
	}
}





#[cfg(test)]
mod tests_binary_prefix_set {
	use super::*;

	#[test]
	fn test_calculate_max_xor() {
		let a = vec![1, 2, 3];
		let mut bt = BinaryPrefixSet::new(3);
	
		for x in &a {
			bt.add(*x);
		}

		assert_eq!(bt.calculate_max_xor(1), 3);
		assert_eq!(bt.calculate_max_xor(2), 3);
		assert_eq!(bt.calculate_max_xor(3), 2);
		assert_eq!(bt.calculate_max_xor(4), 7);

		let mut bt2 = BinaryPrefixSet::new(7);
		let b = vec![14,70,53,83,49,91,36,80,92,51,66,70];
		for x in &b {
			bt2.add(*x);
		}
		assert_eq!(
			b.iter()
			  .map( |x| bt2.calculate_max_xor(*x) )
			  .max(), Some(127)
		);
	}
}
