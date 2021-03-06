use std::fmt;

// Provide bit-wise access to a stored
// number
// b = BitArray(5) == 0b101
// b[0], b[1], b[2] == 1, 0, 1
pub struct BitArray {
	pub number: u32,
	max_width: u32,
}

impl BitArray {
	pub fn new(number: u32, max_width: u32) -> Self {
		BitArray{ number, max_width }
	}


	// ensure idx is between 0..max_width-1
	fn check_idx(&self, idx: u32) -> bool {
		(0 <= idx) && (idx < self.max_width)
	}


	// Get 'idx'th bit
	// idx is 0-indexed MSB to LSB
	pub fn get(&self, idx: u32) -> Option<u32> {
		match self.check_idx(idx) {
			true => {
				let mask = 1 << (self.max_width - idx - 1);
				Some(if self.number & mask != 0 {1} else {0})
			},
			false => None
		}
	}


	// Set 'idx'th bit to 'value'
	// value is either 1 or 0
	// idx is 0-indexed MSB to LSB
	pub fn set(&mut self, idx: u32, val: u32) {
		match self.check_idx(idx) {
			true => {
				let mask = 1 << (self.max_width - idx - 1);
				if val == 1 { self.number |= mask; } else { self.number &= !mask; }
			},
			_ => ()
		}
	}
}

// return the binary string
// representing `number`
// 5 -> '0101'
// fmt::Display provides to_string() and format! for println!
impl fmt::Display for BitArray {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		for idx in 0..self.max_width {
			fmt.write_fmt(format_args!("{}", self.get(idx).unwrap()))?;
		}
		Ok(())
	}
}


#[cfg(test)]
mod tests_bit_array {
	use super::*;

	#[test]
	fn test_parsing() {
		assert_eq!( BitArray::new(3, 5).to_string(),
					"00011"
		);
		let max_width = (10 as f64).log2().ceil(); // pad to 4-bits
		assert_eq!( BitArray::new(5, max_width as u32).to_string(),
					"0101"
		);
	}

	#[test]
	fn test_indexing() {
		let mut b = BitArray::new(5, 6);
		assert_eq!( b.to_string(), "000101" );
		assert_eq!( b.number, 5 );

		assert_eq!( b.get(0).unwrap(), 0 );
		assert_eq!( b.get(1).unwrap(), 0 );
		assert_eq!( b.get(2).unwrap(), 0 );
		assert_eq!( b.get(3).unwrap(), 1 );
		assert_eq!( b.get(4).unwrap(), 0 );
		assert_eq!( b.get(5).unwrap(), 1 );

		b.set(0, 1);
		b.set(5, 0);
		b.set(4, 0); // NO-OP

		assert_eq!( b.to_string(), "100100" );
		assert_eq!( b.number, 36 );
		assert_eq!( b.get(0).unwrap(), 1 );
		assert_eq!( b.get(1).unwrap(), 0 );
		assert_eq!( b.get(2).unwrap(), 0 );
		assert_eq!( b.get(3).unwrap(), 1 );
		assert_eq!( b.get(4).unwrap(), 0 );
		assert_eq!( b.get(5).unwrap(), 0 );
	}

}

