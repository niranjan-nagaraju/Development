/*
Problem 9

(**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))

Example in Haskell:

λ> pack "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]
*/


use std::fmt::Debug;
use std::fmt::Display;
use std::cmp::PartialEq;

use itertools::fold;

pub fn pack<T>( list: &[T]) -> Vec<Vec<T>>
where T: Copy+Debug+Display+PartialEq {
	list.iter()
		.fold(vec![], |mut acc_vv, &item| {
			match acc_vv.iter_mut().last() {
				Some(last_v) => if last_v[0] != item {
						acc_vv.push(vec![item]);
					} else {
						last_v.push(item);
					},
				None => { acc_vv.push(vec![item]); },
			}
			acc_vv
		})
}

pub fn pack2<T>( list: &[T]) -> Vec<Vec<T>>
where T: Copy+Debug+Display+PartialEq {
	fold(list, vec![], |mut acc_vv, &item| {
		match acc_vv.last_mut() {
			Some(last_v) => if last_v[0] != item {
					acc_vv.push(vec![item]);
				} else {
					last_v.push(item);
				},
			None => { acc_vv.push(vec![item]); },
		}
		acc_vv
	})
}

