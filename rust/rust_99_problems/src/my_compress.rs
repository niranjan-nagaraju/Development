/*
Problem 8

(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)

Example in Haskell:

λ> compress "aaaabccaadeeee"
"abcade"

*/
use std::cmp::PartialEq;
use std::fmt::Debug;
use std::fmt::Display;

pub fn compress<T>( list: &[T]) -> Vec<T>
where T: Copy+Debug+Display+PartialEq {
	match list.iter().next() {
		Some(item) => {
			let v: Vec<T> = list
						.iter()
						.skip_while(|x| *x==item)
						.cloned()
						.collect();
			[vec![*item], compress(&v)].concat()
		},
		None => Vec::new(),
	}
}

/* FIXME
// Build goes into an infinite loop
// evaluating skipWhile() recursively
pub fn compress2<T>( list: &[T]) -> Vec<T>
where T: Copy+Debug+Display+PartialEq {
	fn compress2_<'a, T: 'a>( iter : &mut impl Iterator<Item=T> ) -> Vec<T>
	where T: Copy+Debug+Display+PartialEq {
		match iter.next() {
			None => Vec::new(),
			Some(item) => {
				println!("item: {:?}", item);
				[vec![item], compress2_( &mut iter.skip_while(|x| *x==item) )].concat()
			},
		}
	}
	compress2_(&mut list.iter().map(|x| *x))
}
*/


