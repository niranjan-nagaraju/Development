#![allow(dead_code)]
mod my_last;
mod my_but_last;

fn main() {
	println!("99 rust problems!");
}

#[cfg(test)]
mod tests_99_rust_problems {
    #[test]
    fn test_my_last() {
		assert!( crate::my_last::my_last::<i32>(&vec![]) == None );
		assert!( crate::my_last::my_last(&vec![1]) == Some(1) );
		assert!( crate::my_last::my_last(&vec![1,2,3,4,5]).unwrap() == 5 );
		assert!( crate::my_last::my_last(&vec!['a', 'b', 'c']).unwrap() == 'c' );
		assert!( crate::my_last::my_last(&vec!["hello", "world"]).unwrap() == "world" );
    }

    #[test]
    fn test_my_but_last() {
		assert!( crate::my_but_last::my_but_last::<i32>(&vec![]) == None );
		assert!( crate::my_but_last::my_but_last(&vec![1]) == None );
		assert!( crate::my_but_last::my_but_last(&vec![1,2,3,4,5]) == Some(4) );
		assert!( crate::my_but_last::my_but_last(&vec!['a', 'b', 'c']).unwrap() == 'b' );
		assert!( crate::my_but_last::my_but_last(&vec!["hello", "world"]).unwrap() == "hello" );
    }
}
