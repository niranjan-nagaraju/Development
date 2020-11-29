/**
https://www.interviewbit.com/problems/permutations/

Permutations

Given a collection of numbers, return all possible permutations.

Example:

	[1,2,3] will have the following permutations:

	[1,2,3]
	[1,3,2]
	[2,1,3] 
	[2,3,1] 
	[3,1,2] 
	[3,2,1]

NOTE
No two entries in the permutation sequence should be the same.
For the purpose of this problem, assume that all the numbers in the collection are unique.
*/

/*
Solution Outline:
	A simple permutation-generator starts with only the first element,
	  Then at level 2, Makes 2 copies, Inserts second element at indices [0,1]
	  At level 3, Makes 3 copies of the previous level permutations, Inserts third element at indices [0,1,2] for each copy

	e.g.,
	A: [1, 2, 3]
	l0: []
	l1: [1]
	l2: [1] [1] -> [1,2], [2,1]
	l3: [1,2], [2,1] * 3 -> [1,2], [1,2], [1,2], [2,1], [2,1], [2,1]
		-> [1,2,3], [1,3,2], [3,1,2], [2,1,3], [2,3,1], [3,2,1]

	For a backtracking algorithm, Do a DFS traversal, at each (level, i), Add A[level] at index i and backtrack.
	At level == length(A), add current permutation to results list.

  A: [x, y, z]

                                         f([], x, 0):
                    /                                               \
                 f([x], y, 0)                                     f([x], y, 1)
            /          |         \                           /          |        \
f([y,x], z, 0)  f([y,x], z, 1)  f([y,x], z, 2)   f([x,y], z, 0)  f([x,y], z, 1)  f([x,y], z, 2)
  \               \               \                \               \               \    
 [z,y,x]          [y,z,x]         [y,x,z]          [z,x,y]         [x,z,y]         [x,y,z]

*/

struct Solution;

impl Solution {
	pub fn permutations(a: Vec<i32>) -> Vec<Vec<i32>> {
		fn permutations_(a: &Vec<i32>, prefix: Vec<i32>, level: i32, results: &mut Vec<Vec<i32>>) {
			if prefix.len() == a.len() {
				results.push(prefix);
				return;
			}
			for i in 0..prefix.len()+1 {
				permutations_(&a,
					[&prefix[..i], &[a[level as usize]], &prefix[i..]].concat(),
					level+1,
					results);
			}
		}

		let mut results = vec![];
		permutations_(&a, vec![], 0, &mut results);
		results.sort();

		results
	}
}


fn main() {
    assert!( Solution::permutations(vec![]) == [[]] );
    assert!( Solution::permutations(vec![1]) == [[1]] );
    assert!( Solution::permutations(vec![1,2]) == [[1,2], [2,1]] );
	assert!( Solution::permutations(vec![1,2,3]) ==
		[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]] );
}
