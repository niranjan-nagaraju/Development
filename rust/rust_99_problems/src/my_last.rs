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

pub fn my_last<T: Copy>( list: &[T] ) -> T {
	list.iter().fold( list[0], |_,x| *x )
}
