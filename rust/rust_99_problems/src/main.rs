#![allow(dead_code)]
#![allow(unused_imports)]
mod my_last;
mod my_but_last;
mod element_at;
use element_at::element_at;
mod my_length;
use my_length::my_length;
mod my_reverse;
use my_reverse::*;
mod is_palindrome;
use is_palindrome::*;
mod my_flatten;

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

    #[test]
    fn test_my_reverse() {
		use std::str;
		assert!( crate::my_reverse::<i32>(&vec![]) == vec![] );
		assert!( crate::my_reverse(&vec![1]) == vec![1] );
		assert!( crate::my_reverse(&vec![1,2,3,4,5]) == vec![5,4,3,2,1] );
		assert!( crate::my_reverse(&vec!['a', 'b', 'c']) == vec!['c', 'b', 'a'] );
		assert!( crate::my_reverse(&vec!["hello", "world"]) == vec!["world", "hello"] );
		assert!( str::from_utf8( &crate::my_reverse("hello".as_bytes())).unwrap() == "olleh" );
		assert!( str::from_utf8( &crate::my_reverse("A man, a plan, a canal, panama!".as_bytes()) ).unwrap() ==
					"!amanap ,lanac a ,nalp a ,nam A" );

		assert!( crate::my_reverse2::<i32>(&vec![]) == vec![] );
		assert!( crate::my_reverse2(&vec![1]) == vec![1] );
		assert!( crate::my_reverse2(&vec![1,2,3,4,5]) == vec![5,4,3,2,1] );
		assert!( crate::my_reverse2(&vec!['a', 'b', 'c']) == vec!['c', 'b', 'a'] );
		assert!( crate::my_reverse2(&vec!["hello", "world"]) == vec!["world", "hello"] );
		assert!( str::from_utf8( &crate::my_reverse2("hello".as_bytes())).unwrap() == "olleh" );
		assert!( str::from_utf8( &crate::my_reverse2("A man, a plan, a canal, panama!".as_bytes()) ).unwrap() ==
					"!amanap ,lanac a ,nalp a ,nam A" );

		assert!( crate::my_reverse3::<i32>(&vec![]) == vec![] );
		assert!( crate::my_reverse3(&vec![1]) == vec![1] );
		assert!( crate::my_reverse3(&vec![1,2,3,4,5]) == vec![5,4,3,2,1] );
		assert!( crate::my_reverse3(&vec!['a', 'b', 'c']) == vec!['c', 'b', 'a'] );
		assert!( crate::my_reverse3(&vec!["hello", "world"]) == vec!["world", "hello"] );
		assert!( str::from_utf8( &crate::my_reverse3("hello".as_bytes())).unwrap() == "olleh" );
		assert!( str::from_utf8( &crate::my_reverse3("A man, a plan, a canal, panama!".as_bytes()) ).unwrap() ==
					"!amanap ,lanac a ,nalp a ,nam A" );
    }

    #[test]
    fn test_is_palindrome() {
		assert!( crate::is_palindrome::<i32>(&vec![]) == true );
		assert!( crate::is_palindrome(&vec![1]) == true );
		assert!( crate::is_palindrome(&vec![1,2,3,4,5]) == false );
		assert!( crate::is_palindrome(&vec![1,2,3,2,1]) == true );
		assert!( crate::is_palindrome(&vec![1,2,2,1]) == true );
		assert!( crate::is_palindrome("racecar".as_bytes()) );

		assert!( crate::is_palindrome2::<i32>(&vec![]) == true );
		assert!( crate::is_palindrome2(&vec![1]) == true );
		assert!( crate::is_palindrome2(&vec![1,2,3,4,5]) == false );
		assert!( crate::is_palindrome2(&vec![1,2,3,2,1]) == true );
		assert!( crate::is_palindrome2(&vec![1,2,2,1]) == true );
		assert!( crate::is_palindrome2("racecar".as_bytes()) );

		assert!( crate::is_palindrome3::<i32>(&vec![]) == true );
		assert!( crate::is_palindrome3(&vec![1]) == true );
		assert!( crate::is_palindrome3(&vec![1,2,3,4,5]) == false );
		assert!( crate::is_palindrome3(&vec![1,2,3,2,1]) == true );
		assert!( crate::is_palindrome3(&vec![1,2,2,1]) == true );
		assert!( crate::is_palindrome3("racecar".as_bytes()) );
    }

    #[test]
    fn test_flatten() {
		use crate::my_flatten::*;
		assert!( my_flatten::<i32>( &List(vec![]) ) == vec![] );
		assert!( my_flatten( &Elem(100) ) == vec![100] );
		assert!( my_flatten( &List( vec![Elem(100), List( vec![Elem(200)] )] ) ) == vec![100, 200] );
		assert!( my_flatten( &List( vec![Elem(1),
								List( vec![Elem(2),
									List( vec![Elem(3), Elem(4)] ),
									Elem(5)]
								)] ) ) == vec![1,2,3,4,5] );
		assert!( my_flatten( &List( vec![List( vec![List( vec![Elem(1)] )] )] ) ) == [1].to_vec() ); 
	}
}
