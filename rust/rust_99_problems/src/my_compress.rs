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


