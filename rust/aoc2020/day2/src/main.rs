use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct PasswordPolicy {
	min: i32,
	max: i32,
	passwd_char: char,
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
		let passwd_char = code.chars().nth(minus_idx+space_idx+2).unwrap();

		// skip 4 characters, ' <char>: ' to get the password
		let password = code[minus_idx+space_idx+5..].to_owned();

		Ok(PasswordPolicy { min, max, passwd_char, password })
	}
}


fn main() {
	let code: &str = &r"12-35466 a: abcdef";

	match PasswordPolicy::from_str(code) {
		Ok(x) => println!("found {:?}", x),
			Err(e) => println!("Error: {}", e),
	}
}
