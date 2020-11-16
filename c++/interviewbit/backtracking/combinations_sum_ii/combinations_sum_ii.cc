/**
https://www.interviewbit.com/problems/combination-sum-ii/

Combination Sum II

Given a collection of candidate numbers (C) and a target number (T), find all unique combinations in C where the candidate numbers sums to T.

Each number in C may only be used once in the combination.

Note:
 All numbers (including target) will be positive integers.
 Elements in a combination (a1, a2, … , ak) must be in non-descending order. (ie, a1 ≤ a2 ≤ … ≤ ak).
 The solution set must not contain duplicate combinations.
 
Example :
 Given candidate set 10,1,2,7,6,1,5 and target 8,

A solution set is:
	 [1, 7]
	 [1, 2, 5]
	 [2, 6]
	 [1, 1, 6]
*/


/**
Solution Outline:
	1. For each x in C, x is either part of the solution set, or it isn't.
	2. Let f(c, t) be a function that returns a solution for target sum, t, using c as the candidate set.
		Recursively solve f(c, t) as
			f(c-x, t-x) + f(c-x, t), for each x in C
		=> f(c-x, t-x): Solution set that includes x, 
		=> f(c-x, t): Solution set that excludes x.


Sample run:
	C: [1,2,5,7], target: 8

	L0: f([1,2,5,7], 8, []):
		L1: f([2,5,7], 7, [1]) + f([2,5,7], 8, [])
		L1: f([2,5,7, 7, [1])
			L2: f([5,7], 5, [1,2]) + f([5,7], 7, [1])
			L2: f([5,7], 5, [1,2])
				L3: f([7], 0, [1,2,5]) + f([7], 5, [1,2])
				L3: f([7], 0, [1,2,5]) <-- [1,2,5] is a candidate
				L3: f([7], 5, [1,2]) = f([], -2, [1,2,7]) + f([], 5, [1,2]) == []
			L2: f([5,7], 7, [1])
				L3: f([7], 2, [1,5]) + f([7], 7, [1])
				L3: f([7], 2, [1,5]) == []
				L3: f([7], 7, [1]) = f([], 0, [1,7]) <--- [1,7] is a candidate
		L1: f([2,5,7, 8, []) == []

	Solution: [[1,2,5], [1,7]]

*/

#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <cassert>

using VecInt = std::vector<int>;
using VecVecInt = std::vector<VecInt>;
using SetVecInt = std::set<VecInt>;

class Solution {
	private:
		void combinations_sum(VecInt C, int target, VecInt &prefix, SetVecInt &results);
	public:
		VecVecInt combinations_sum(VecInt &C, int target);
};


void
Solution::combinations_sum(VecInt C, int target, VecInt &prefix, SetVecInt &results)
{
	if (target == 0) {
		// target-sum can achieved with prefix+C[0]
		// add to results, and backtrack to previous levels
		// backtrack to previous levels
		// because current level can only
		// have 1 candidate to match the target sum
		// NOTE: c can be [] at this point
		// so check if target sum was achieved first
		VecInt result(prefix);
		results.insert(result);
		return;
	}

	if (!C.size() || target < 0) {
		// backtrack to previous levels
		// c is [] or
		// the lowest number in C > required target sum
		return;
	}
	prefix.push_back(C[0]);
	// Include C[0]
	combinations_sum( VecInt(C.begin()+1, C.end()), target-C[0], prefix, results);
	prefix.pop_back();

	// Exclude C[0]
	combinations_sum( VecInt(C.begin()+1, C.end()), target, prefix, results );
}

VecVecInt
Solution::combinations_sum(VecInt &C, int target)
{
	SetVecInt results;
	VecInt prefix;

	std::sort(C.begin(), C.end());

	// A set is used to de-dup entries incase C itself has duplicates
	// for e.g., if x == x',
	// in which case, [x, x', y] and [x', x, y] will be duplicate
	// entries in the results
	combinations_sum(C, target, prefix, results);
	return VecVecInt(results.begin(), results.end()); 
}


int
main(void)
{
	Solution s;
	{
		VecInt v = {4,3,2,7};
		assert( s.combinations_sum(v, 7) == (VecVecInt{{3,4}, {7}}) );
		assert( v == (VecInt{2,3,4,7}) );
	}

	{
		VecInt v = {2,3,6,7};
		assert( s.combinations_sum(v, 7) ==	(VecVecInt{{7}}) );
	}
	{
		auto v = VecInt{10,1,2,7,6,1,5};
		assert( s.combinations_sum(v, 8) == (VecVecInt{{1,1,6}, {1,2,5}, {1,7}, {2,6}}) );
	}
	{
		auto v = VecInt{1,2,5,6,7,8};
		assert( s.combinations_sum(v, 8) == (VecVecInt{{1,2,5}, {1,7}, {2,6}, {8}}) );
	}

	return 0;
}

