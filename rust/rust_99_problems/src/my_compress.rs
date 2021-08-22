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
use itertools::fold;

// Accumulate current item into a unique-items vector.
// skip ahead all items == current item
// collect the remaining items into a vector
// recursively compress ( remaining items vector )
// Return the accumulated unique items vector.
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

// Accumulate into unique items vector
// as long as current item != last item in the unique items vector
// weeding out all the consecutive repeats.
pub fn compress2<T>( list: &[T]) -> Vec<T>
    where T: Copy+Debug+Display+PartialEq {
    list.iter()
        .fold(vec![], |mut acc_v, &item| {
            match acc_v.last() {
                Some(&last) => if last != item { acc_v.push(item); },
                None => { acc_v.push(item); },
            }
            acc_v
        })
}

// using slices
pub fn compress3<T: Copy+PartialEq+Debug+Display>(list: &[T]) -> Vec<T> {
	pub fn compress3_<T: Copy+PartialEq+Debug+Display>(list: &[T], packed_list: &mut Vec<T>) {
		match list {
			[] => (),
			[first, rest @ ..] => {
				let last = packed_list.last();
				if Some(first) != last || last == None {
					// either current item != last seen
					// *OR*
					// "last seen" is empty => this is the first item
					// in the original list
					packed_list.push(*first);
				}
				compress3_( &rest, packed_list );
			},
		}
	}

	let mut v = vec![];
	compress3_(list, &mut v);
	v
}

// using fold
pub fn compress4<T: Copy+PartialEq+Debug+Display>(list: &[T]) -> Vec<T> {
	fold(list, vec![], |mut acc_v, item| {
		let last = acc_v.last();
		if Some(item) != last || last == None {
			acc_v.push(*item);
		}
		acc_v
	})
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


