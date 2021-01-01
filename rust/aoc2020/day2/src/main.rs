/*
 https://adventofcode.com/2020/day/2

 --- Day 2: Password Philosophy ---
 Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

Your puzzle answer was 524.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---
While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
How many passwords are valid according to the new interpretation of the policies?

*/

use std::str::FromStr;
use std::io::{self, BufRead};

#[derive(Debug, PartialEq)]
struct PasswordPolicy {
	min: i32,
	max: i32,
	policy_char: char,
	password: String,
}

impl FromStr for PasswordPolicy {
	type Err = String; 
	fn from_str(code: &str) -> Result<Self, String> {
		// Find '-'
		let minus_idx = code.find('-')
			.ok_or_else(|| "Invalid format - Couldn't locate '-'")?;
		let min = i32::from_str_radix(&code[0..minus_idx], 10)
			.map_err(|err| err.to_string())?;

		// Find '<space>'
		let space_idx = code[minus_idx+1 ..].find(' ')
			.ok_or_else(|| "Invalid format - Couldn't locate ' '")?;
		let max = i32::from_str_radix(&code[minus_idx+1..minus_idx+space_idx+1], 10)
			.map_err(|err| err.to_string())?;

		// skip space to get character in password policy 
		let policy_char = code.chars().nth(minus_idx+space_idx+2).unwrap();

		// skip 4 characters, ' <char>: ' to get the password
		let password = code[minus_idx+space_idx+5..].to_owned();

		Ok(PasswordPolicy { min, max, policy_char, password })
	}
}

impl PasswordPolicy {
	// Validate password according to policy
	// Password is valid if `policy_char` appears
	// between `min` .. `max` times in the password.
	pub fn valid(&self) -> bool {
		(self.min .. self.max+1)
			.contains(&(
					self.password
					.chars()
					.filter(|&c| c == self.policy_char)
					.count() as i32)
			)
	}
}

fn main() {
	let stdin = io::stdin();
	let valid_passwords = stdin
		.lock()
		.lines()
		.filter_map(|line_result| line_result.ok())
		.filter_map(|line| PasswordPolicy::from_str(line.as_str()).ok())
		.filter(|ppolicy| ppolicy.valid())
		.count();

	println!("Valid passwords: {:#?}", valid_passwords);
}


#[cfg(test)]
mod tests_day2 {
	use super::*;

    #[test]
    fn basic_tests_day2() {
        assert_eq!(
			PasswordPolicy::from_str("12-34 c: abcdef").unwrap(),
			PasswordPolicy{
				min: 12,
				max: 34,
				policy_char: 'c',
				password: "abcdef".to_string(),
			}
		);

        assert_eq!(
			PasswordPolicy::from_str("12*34 c: abcdef"),
			Err("Invalid format - Couldn't locate '-'".to_string())
		);

		let p = PasswordPolicy::from_str("12-34 c: abcdef").unwrap();
		assert_eq!(p.valid(), false);

		let p2 = PasswordPolicy::from_str("2-34 b: abcbef").unwrap();
		assert_eq!(p2.valid(), true);

		let p3 = PasswordPolicy::from_str("2-34 x: abcbef").unwrap();
		assert_eq!(p3.valid(), false);
    }
}

