/*
Problem 3

(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c

Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
*/


pub fn element_at<T: Copy>( list: &[T], idx: usize ) -> Option<T> {
	if idx < 1 {
		return None;
	}

	// skip() will be an empty iterator and therefore skip().next will be None if `idx`
	// turned out to be bigger than the list size
	match list.iter()
			  .skip(idx-1)
			  .next() {
		None => None,
		Some(&x) => Some(x),
	}
}

// using (x:xs) slice pattern
pub fn element_at2<T: Copy>(list: &[T], idx: usize) -> Option<T> {
	if idx == 0 {
		// usize cannot be -ve
		return None;
	}
	match list {
		[] => None,
		[ first, rest @ .. ] => if idx == 1 { Some(*first) } else { element_at2(rest, idx-1) },
	}
}
