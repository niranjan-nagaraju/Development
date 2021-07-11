/*
Problem 7

(**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:
* (my-flatten '(a (b (c d) e)))
* (A B C D E)
* Example in Haskell:
*
* We have to define a new data type, because lists in Haskell are homogeneous.
*
*  data NestedList a = Elem a | List [NestedList a]
*  λ> flatten (Elem 5)
*  [5]
*  λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
*  [1,2,3,4,5]
*  λ> flatten (List [])
*  []
*/

use std::fmt::Debug;

#[derive(Debug)]
pub enum NestedList<T> {
    Elem(T),
    List(Vec<NestedList<T>>),
}

pub use NestedList::Elem as Elem;
pub use NestedList::List as List;

pub fn my_flatten<T: Copy + Debug>( nl: &NestedList<T>) -> Vec<T> {
    match nl {
        Elem(x) => vec![*x],
        List(v) => v
				.iter()
				.fold(
					Vec::new(),
					|acc_v, inner_list| [acc_v, my_flatten(inner_list)].concat()
				),
    } 
}

