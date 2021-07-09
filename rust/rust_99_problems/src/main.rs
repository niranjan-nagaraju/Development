#![allow(dead_code)]
mod my_last;
mod my_but_last;
mod element_at;
use element_at::element_at;
mod my_length;
use my_length::my_length;

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

    #[test]
    fn test_element_at() {
		assert!( crate::element_at::<i32>(&vec![], 1) == None );
		assert!( crate::element_at(&vec![1], 2) == None );
		assert!( crate::element_at(&vec![1], 1).unwrap() == 1 );
		assert!( crate::element_at(&vec![1,2,3,4,5], 4) == Some(4) );
		assert!( crate::element_at(&vec![1,2,3,4,5], 1) == Some(1) );
		assert!( crate::element_at(&vec![1,2,3,4,5], 0) == None );
		assert!( crate::element_at(&vec![1,2,3,4,5], 5) == Some(5) );
		assert!( crate::element_at(&vec![1,2,3,4,5], 6) == None );
		assert!( crate::element_at(&vec!['a', 'b', 'c'], 2).unwrap() == 'b' );
		assert!( crate::element_at(&vec!["hello", "world"], 1).unwrap() == "hello" );
		assert!( crate::element_at(&vec!["hello", "world"], 2).unwrap() == "world" );
    }

    #[test]
    fn test_my_length() {
		assert!( crate::my_length::<i32>(&vec![]) == 0 );
		assert!( crate::my_length(&vec![1]) == 1 );
		assert!( crate::my_length(&vec![1,2,3,4,5]) == 5 );
		assert!( crate::my_length(&vec!['a', 'b', 'c']) == 3 );
		assert!( crate::my_length(&vec!["hello", "world"]) == 2 );
		assert!( crate::my_length("hello".as_bytes()) == 5 );
    }
}
