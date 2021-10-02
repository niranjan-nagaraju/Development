/*
Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

Example in Haskell:

λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
*/

use std::fmt::Debug;
use std::fmt::Display;
use std::cmp::PartialEq;
use crate::my_pack::pack;

// using pack
pub fn encode<T>( list: &[T]) -> Vec<(u32, T)>
where T: Copy+Debug+Display+PartialEq {
    pack(&list)
		.iter()
		.map(|inner| (inner.len() as u32, inner[0]))
		.collect()
}


// using pack and (x:xs) recursion
pub fn encode2<T>( list: &[T]) -> Vec<(u32, T)>
where T: Copy+Debug+Display+PartialEq {
    fn encode_helper<T>(list: &[Vec<T>], out: &mut Vec<(u32, T)>)
    where T: Copy+Debug+Display+PartialEq {
        match list {
            [] => (),
            [first, rest @ ..] => {
				out.push((first.len() as u32, first[0]));
				encode_helper(rest, out);
			},
        }
    }
    let packed : Vec<Vec<T>> = pack(&list);
    let mut out = Vec::new();
    encode_helper(&packed, &mut out);
    out
} 


