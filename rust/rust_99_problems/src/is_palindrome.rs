/*
Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True
*/

//mod my_reverse;
use crate::my_reverse::my_reverse2 as my_reverse;

// reverse the list and compare the original and reversed list
pub fn is_palindrome<T: Copy + std::cmp::PartialEq>( list: &[T] ) -> bool {
	my_reverse(list)
		.iter()
		.zip(list.iter())
		.all(|(&a, &b)| a == b)
}

// Compare forward and reverse iterators
pub fn is_palindrome2<T: Copy + std::cmp::PartialEq>( list: &[T] ) -> bool {
	list
		.iter()
		.zip(list.iter().rev())
		.all(|(&a, &b)| a == b)
}

// Compare forward and reverse iterators
// but only till half-way point
pub fn is_palindrome3<T: Copy + std::cmp::PartialEq>( list: &[T] ) -> bool {
	list
		.iter()
		.take(list.len()/2)
		.zip(list
				.iter()
				.rev()
				.take(list.len()/2)
			)
		.all(|(&a, &b)| a == b)
}


// using slices
pub fn is_palindrome4<T: Copy + std::cmp::PartialEq>( list: &[T] ) -> bool {
	match list {
		[] | [_] => true,
		[first, mid @ .., last] => (first == last) && is_palindrome4(mid),
	}
}
