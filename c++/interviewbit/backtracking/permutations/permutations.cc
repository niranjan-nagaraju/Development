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

/**
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

#include "permutations.h"

VecVecInt
Solution::permute(VecInt &A)
{
	VecVecInt permutations_list;
	VecInt prefix;
	permute(A, prefix, 0, permutations_list);
	std::sort(permutations_list.begin(), permutations_list.end());

	return permutations_list;
}


void
Solution::permute(VecInt &A, VecInt &prefix, int level, VecVecInt &results)
{
	if (prefix.size() == A.size()) {
		results.push_back(VecInt(prefix));
		return;
	}

	for (auto i=0; i<=prefix.size(); i++) {
		// Slicing prefix to a new array while inserting
		// A[level] will take O(n) - same as insert(idx)+pop(idx)
		// So we might as well save some memory modifying prefix vs copying
		prefix.insert(prefix.begin()+i, A[level]);
		permute(A, prefix, level+1, results);
		prefix.erase(prefix.begin()+i); //backtrack
	}
}

int
main(void)
{
    Solution s;
	{
		VecInt v;
		assert( s.permute(v) == (VecVecInt{{}}) );
	}
	{
		VecInt v = {1};
		assert( s.permute(v) == (VecVecInt{{1}}) );
	}
	{
		VecInt v = {1,2};
		assert( s.permute(v) == (VecVecInt{{1,2}, {2,1}}) );
	}
	{
		VecInt v = {1,2,3};
		assert( s.permute(v) == (VecVecInt{
					{1,2,3},
					{1,3,2},
					{2,1,3},
					{2,3,1},
					{3,1,2},
					{3,2,1},
				}) );
	}

	return 0;
}
