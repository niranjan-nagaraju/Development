/*
Problem 5
(*) Reverse a list.

Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
*/

pub fn my_reverse<T: Copy>( list: &[T] ) -> Vec<T> {
    if list.len() == 0 {
        return list.to_vec();
    }
    match list.split_at(1) {
        ([x], []) => [*x].to_vec(),
        (head, tail) => [&my_reverse(tail), head].concat(),
    }
}
