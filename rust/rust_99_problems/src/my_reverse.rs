/*
Problem 5
(*) Reverse a list.

Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
*/


use itertools::fold;

// Move head of each iteration to the end
// creating a reversed list
pub fn my_reverse<T: Copy>( list: &[T] ) -> Vec<T> {
    if list.len() == 0 {
        return list.to_vec();
    }
    match list.split_at(1) {
        ([x], []) => [*x].to_vec(),
        (head, tail) => [&my_reverse(tail), head].concat(),
    }
}

// Use a reverse iterator, collecting the items
// into a list in the reverse order
pub fn my_reverse2<T: Copy>( list: &[T] ) -> Vec<T> {
    list
		.iter()
		.rev()
		.cloned()
		.collect()
}


// Reverse using two-pointers
// reverse([a, mid.., b]) -> [b, reverse(mid), a] 
pub fn my_reverse3<T: Copy>( list: &[T] ) -> Vec<T> {
	if list.len() == 0 {
		return Vec::new();
	}
	match (
		list.iter()
			.next(), 
		list.iter()
			.take(list.len()-1)
			.skip(1)
			.cloned()
			.collect::<Vec<T>>(), 
		list.iter()
			.skip(1)
			.last()) {
			(Some(&first), mid, Some(&last)) => [
						&[last],
						my_reverse(&mid).as_slice(),
						&[first]
					].concat(),
			(Some(&item), _, None) => vec![item],
			(None, _, _) => Vec::new(),
	}
}

// using fold
pub fn my_reverse4<T: Copy>( list: &[T] ) -> Vec<T> {
	fold(list, vec![], |acc,x| [vec![*x], acc].concat())
}

// using slices, and recursively appending `head` back into tail
pub fn my_reverse5<T: Copy>( list: &[T] ) -> Vec<T> {
	match list {
		[] => vec![],
		[first, tail @ ..] =>[my_reverse5(tail), vec![*first]].concat(),
	}
}
