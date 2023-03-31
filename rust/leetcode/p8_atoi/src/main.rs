/*
8. String to Integer (atoi)
https://leetcode.com/problems/string-to-integer-atoi/

Implement the myAtoi(string s) function, which converts a string to a 32-bit signed integer (similar to C/C++'s atoi function).

The algorithm for myAtoi(string s) is as follows:

Read in and ignore any leading whitespace.
Check if the next character (if not already at the end of the string) is '-' or '+'. Read this character in if it is either.
This determines if the final result is negative or positive respectively. Assume the result is positive if neither is present.
Read in next the characters until the next non-digit character or the end of the input is reached. The rest of the string is ignored.
Convert these digits into an integer (i.e. "123" -> 123, "0032" -> 32). If no digits were read, then the integer is 0. Change the sign as necessary (from step 2).
If the integer is out of the 32-bit signed integer range [-2^31, 2^31 - 1], then clamp the integer so that it remains in the range. Specifically, integers less than -2^31 should be clamped to -2^31, and integers greater than 2^31 - 1 should be clamped to 2^31 - 1.
Return the integer as the final result.

Note:
Only the space character ' ' is considered a whitespace character.
Do not ignore any characters other than the leading whitespace or the rest of the string after the digits.

*/

struct Solution {}

impl Solution {
	pub fn my_atoi(s: String) -> i32 {
		let mut num:i32 = 0;
		let mut sign = 1;
		let chr_to_num = |c: char| (c as i32) - ('0' as i32);

		// trim leading spaces
		let ss = s.trim_start_matches(' ');
		if ss == "" { return 0; }
		// split leading character and the rest of the numbers
		let (leading, rest) = ss.split_at( 1 );
		let leading_char = leading.chars().next().unwrap();
		match leading_char {
			'0'..='9' => num = chr_to_num(leading_char),
			'+' => sign = 1,
			'-' => sign = -1,
			'.'| _ => return 0, // Invalid Input or a leading decimal ".123" => 0
		}
		let mut idx = 0;
		for c in rest.chars() {
			match c{
				'0'..='9' => {
					// num : Running total so far
					// new-number would be num * 10 + int(c)
					// Check if we have already overflowed 2**31 by checking num*10 > 2**31
					// or if num**10+c > 2**31
					if num > ( i32::MAX / 10 ) {
						return if sign == -1 {i32::MIN} else {i32::MAX};
					} else if num == ( i32::MAX / 10 ) {
						let mut digit = chr_to_num(c);
						let max_digit = if sign == -1 { i32::MAX % 10 + 1 } else { i32::MAX % 10 };
						if digit > max_digit {
							digit = max_digit;
						}
						if idx+1 < rest.len() {
							let lookahead = rest.chars().nth(idx+1).unwrap();
							if lookahead >= '0' && lookahead <= '9' {
								// for e.g, "21474836460" where at this point
								// num == (1<<31)/10 but it's also a significantly big number
								digit = max_digit;
							}
						}
						return sign * (num * 10 + digit);
					} else { // num * 10 < 2**31
						// update running total
						num = num * 10 + chr_to_num(c);
					}
				},
				'.' | ' ' | _ => return sign * num,
			} 
			idx += 1;
		}
		sign * num
	}
}

fn main() {
	println!("Run program!");
	println!( "atoi({}) : {}", "42", Solution::my_atoi( "42".to_string() ) );
}

#[cfg(test)]
mod basic_tests {
	use super::*;

#[test]
	fn basic_tests() {
		assert_eq!(Solution::my_atoi( "42".to_string() ), 42);
		assert_eq!(Solution::my_atoi( "+42".to_string() ), 42);
		assert_eq!(Solution::my_atoi( "-42".to_string() ), -42);
		assert_eq!(Solution::my_atoi( " -1337 ".to_string() ), -1337);
		assert_eq!(Solution::my_atoi( "4193 with words".to_string() ), 4193);
		assert_eq!(Solution::my_atoi( "-91283472332".to_string() ), i32::MIN );
		assert_eq!(Solution::my_atoi( "91283472332".to_string() ), i32::MAX );
		assert_eq!(Solution::my_atoi( "3.1456".to_string() ), 3 );
		assert_eq!(Solution::my_atoi( "words 1234".to_string() ), 0 );
		assert_eq!(Solution::my_atoi( "21474836460".to_string() ), i32::MAX );
	}
}
