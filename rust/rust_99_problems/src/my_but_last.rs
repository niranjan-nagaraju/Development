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


// using fold to store (previous item, current item)
// as a "zip" but replacing the pair at every iteration.
// return 'x' from the last stored (x,y)
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

// using slices
pub fn my_but_last2<T: Copy>( list: &[T] ) -> Option<T> {
	match list {
		[] | [_]  => None,
		[one, _] => Some(*one),
		[.., last_but_one, _] => Some(*last_but_one),
	}
}

// using (x:xs) slice, and recursively calling f(xs) until (x,y)
pub fn my_but_last3<T: Copy>( list: &[T] ) -> Option<T> {
	match list {
		[] | [_]  => None,
		[one, _] => Some(*one),
		[_, rest @ .. ] => my_but_last3(rest),
	}
}

