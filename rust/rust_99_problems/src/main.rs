#[allow(dead_code)]
mod my_last;

fn main() {
	println!("99 rust problems!");
}

#[cfg(test)]
mod tests_99_rust_problems {
    #[test]
    fn test_my_last() {
		assert!( crate::my_last::my_last(&vec![1,2,3,4,5]) == 5 );
		assert!( crate::my_last::my_last(&vec!['a', 'b', 'c']) == 'c' );
		assert!( crate::my_last::my_last(&vec!["hello", "world"]) == "world" );
    }
}
