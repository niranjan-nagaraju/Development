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
mod my_compress;
mod my_pack;
mod encode;

fn main() {
	println!("99 rust problems!");
}

#[cfg(test)]
mod tests_99_rust_problems {
    #[test]
    fn test_my_last() {
		use std::char;
		assert!( crate::my_last::my_last::<i32>(&vec![]) == None );
		assert!( crate::my_last::my_last(&vec![1]) == Some(1) );
		assert!( crate::my_last::my_last(&vec![1,2,3,4,5]).unwrap() == 5 );
		assert!( crate::my_last::my_last(&vec!['a', 'b', 'c']).unwrap() == 'c' );
		assert!( crate::my_last::my_last(&vec!["hello", "world"]).unwrap() == "world" );
		assert!( char::from_u32( crate::my_last::my_last("abcdef".as_bytes() ).unwrap() as u32 ) == Some('f') );

		assert!( crate::my_last::my_last2::<i32>(&vec![]) == None );
		assert!( crate::my_last::my_last2(&vec![1]) == Some(1) );
		assert!( crate::my_last::my_last2(&vec![1,2,3,4,5]).unwrap() == 5 );
		assert!( crate::my_last::my_last2(&vec!['a', 'b', 'c']).unwrap() == 'c' );
		assert!( crate::my_last::my_last2(&vec!["hello", "world"]).unwrap() == "world" );
		assert!( char::from_u32( crate::my_last::my_last2("abcdef".as_bytes() ).unwrap() as u32 ) == Some('f') );

		assert!( crate::my_last::my_last3::<i32>(&vec![]) == None );
		assert!( crate::my_last::my_last3(&vec![1]) == Some(1) );
		assert!( crate::my_last::my_last3(&vec![1,2,3,4,5]).unwrap() == 5 );
		assert!( crate::my_last::my_last3(&vec!['a', 'b', 'c']).unwrap() == 'c' );
		assert!( crate::my_last::my_last3(&vec!["hello", "world"]).unwrap() == "world" );
		assert!( char::from_u32( crate::my_last::my_last3("abcdef".as_bytes() ).unwrap() as u32 ) == Some('f') );
    }

    #[test]
    fn test_my_but_last() {
		use std::char;
		assert!( crate::my_but_last::my_but_last::<i32>(&vec![]) == None );
		assert!( crate::my_but_last::my_but_last(&vec![1]) == None );
		assert!( crate::my_but_last::my_but_last(&vec![1,2,3,4,5]) == Some(4) );
		assert!( crate::my_but_last::my_but_last(&vec!['a', 'b', 'c']).unwrap() == 'b' );
		assert!( crate::my_but_last::my_but_last(&vec!["hello", "world"]).unwrap() == "hello" );
		assert!( char::from_u32( crate::my_but_last::my_but_last("abcdef".as_bytes() ).unwrap() as u32 ) == Some('e') );

		assert!( crate::my_but_last::my_but_last2::<i32>(&vec![]) == None );
		assert!( crate::my_but_last::my_but_last2(&vec![1]) == None );
		assert!( crate::my_but_last::my_but_last2(&vec![1,2,3,4,5]) == Some(4) );
		assert!( crate::my_but_last::my_but_last2(&vec!['a', 'b', 'c']).unwrap() == 'b' );
		assert!( crate::my_but_last::my_but_last(&vec!["hello", "world"]).unwrap() == "hello" );
		assert!( char::from_u32( crate::my_but_last::my_but_last2("abcdef".as_bytes() ).unwrap() as u32 ) == Some('e') );

		assert!( crate::my_but_last::my_but_last3::<i32>(&vec![]) == None );
		assert!( crate::my_but_last::my_but_last3(&vec![1]) == None );
		assert!( crate::my_but_last::my_but_last3(&vec![1,2,3,4,5]) == Some(4) );
		assert!( crate::my_but_last::my_but_last3(&vec!['a', 'b', 'c']).unwrap() == 'b' );
		assert!( crate::my_but_last::my_but_last3(&vec!["hello", "world"]).unwrap() == "hello" );
		assert!( char::from_u32( crate::my_but_last::my_but_last3("abcdef".as_bytes() ).unwrap() as u32 ) == Some('e') );
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

	#[test]
    fn test_compress() {
		use crate::my_compress::*;
		use std::str;
		assert!( compress::<i32>(&vec![]) == vec![] );
		assert!( compress(&vec![1,1,1,2,2,3,3,3,3,4,3,4]) == vec![1,2,3,4,3,4] );
		assert!( compress(&vec![1,1,1,2,2,3,3,3,3,4,4,4]) == vec![1,2,3,4] );
		assert!( str::from_utf8( &compress("aabbcdabb".as_bytes()) ).unwrap() == "abcdab" );

		assert!( compress2::<i32>(&vec![]) == vec![] );
		assert!( compress2(&vec![1,1,1,2,2,3,3,3,3,4,3,4]) == vec![1,2,3,4,3,4] );
		assert!( compress2(&vec![1,1,1,2,2,3,3,3,3,4,4,4]) == vec![1,2,3,4] );
		assert!( str::from_utf8( &compress2("aabbcdabb".as_bytes()) ).unwrap() == "abcdab" );
	}

	#[test]
	fn test_pack() {
		use crate::my_pack::*;
		use std::str;

		// Convert a nested list of utf-8 codes to a list of strings.
		// [[97,97], [98,98]] -> ["aa", "bb"]
		fn to_string_vec( slist: &Vec<Vec<u8>> ) -> Vec<String> {
			slist
				.iter()
				.map(|inner_list| str::from_utf8(inner_list).unwrap().to_string())
				.collect::<Vec<String>>()
		}
		assert!( pack::<i32>(&vec![]) == vec![] as Vec<Vec<i32>>);
		assert!( pack(&vec![1,1,1,2,2,3,3,3,3,4,3,4]) == vec![vec![1,1,1], vec![2,2], vec![3,3,3,3], vec![4], vec![3], vec![4]] );
		assert!( pack(&vec![1,1,1,2,2,3,3,3,3,4,4,4]) == vec![vec![1,1,1], vec![2,2], vec![3,3,3,3], vec![4,4,4]] );
		assert!( to_string_vec(&pack("aabbcdabb".as_bytes()) ) == vec!["aa", "bb", "c", "d", "a", "bb"] );
		assert!( to_string_vec(&pack("aaaabccaadeeee".as_bytes()) ) == vec!["aaaa","b","cc","aa","d","eeee"] );
	}

	#[test]
	fn test_encode() {
		use crate::encode::*;
		// Convert [(u32, utf8-code)] -> [(u32, char)]
		fn from_utf8_to_char_in_tuples(list: &Vec<(u32, u8)>) -> Vec<(u32, char)> {
			list
				.iter()
				.fold( Vec::new(),
					   |mut acc_v, (x,y)| {
						 acc_v.push((*x, *y as char)); //u can be cast as char
						 acc_v
						}
					)
		}

		assert!( encode::<i32>(&vec![]) == vec![] );
		assert!( encode(&vec![1,1,1,2,2,3,3,3,3,4,3,4]) == vec![(3,1), (2,2), (4,3), (1,4), (1,3), (1,4)] );
		assert!( encode(&vec![1,1,1,2,2,3,3,3,3,4,4,4]) == vec![(3,1), (2,2), (4,3), (3,4)] );
		assert!( from_utf8_to_char_in_tuples(&encode("aabbcdabb".as_bytes()))  ==
			vec![(2,'a'), (2,'b'), (1,'c'), (1,'d'),(1,'a'),(2,'b')] );
		assert!( from_utf8_to_char_in_tuples(
			&encode("aaaabccaadeeee".as_bytes())) == vec![(4,'a'), (1,'b'), (2,'c'),(2,'a'),(1,'d'),(4,'e')] );
	}
}
