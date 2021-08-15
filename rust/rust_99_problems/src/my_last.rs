/*
Problem 1

(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
*/

// using fold
pub fn my_last<T: Copy>( list: &[T] ) -> Option<T> {
	if list.len() < 1 {
		None
	} else {
		Some( list.iter().fold( list[0], |_,x| *x ) )
	}
}

// using [first, mid, last] slice
pub fn my_last2<T : Copy>( list: &[T] ) -> Option<T> {
	match list {
		[_first, _middle @ .., last] => Some(*last),
		[item] => Some(*item),
		[] => None,
	}
}

// using (x:xs) and recursively calling f(xs)
// until a last item can be resolved
pub fn my_last3<T>(items: &[T]) -> Option<T>
where T: Copy {
	match items {
		[] => None,
		[item] => Some(*item),
		[_first, rest @ .. ] => my_last3(rest),
	}
}

