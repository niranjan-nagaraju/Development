/*
Problem 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
*/


pub fn my_but_last<T: Copy>( list: &[T] ) -> Option<T> {
	if list.len() < 2 {
		None
	} else { 
		Some(
			list.iter()
				.fold( (list[0],list[1]), |acc,x| (acc.1, *x) ).0
		)
	}
}
