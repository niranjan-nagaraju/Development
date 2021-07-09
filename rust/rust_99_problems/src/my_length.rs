/*
Problem 4

(*) Find the number of elements of a list.

Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
*/

pub fn my_length<T: Copy>( list: &[T] ) -> usize {
	list.iter().count()
}
