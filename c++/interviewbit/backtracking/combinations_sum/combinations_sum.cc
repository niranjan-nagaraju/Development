/**
https://www.interviewbit.com/problems/combination-sum/

Combination Sum

Given a set of candidate numbers (C) and a target number (T), find all unique combinations in C where the candidate numbers sums to T.

The same repeated number may be chosen from C unlimited number of times.

 Note:
 All numbers (including target) will be positive integers.
 Elements in a combination (a1, a2, … , ak) must be in non-descending order. (ie, a1 ≤ a2 ≤ … ≤ ak).
 The combinations themselves must be sorted in ascending order.
 CombinationA > CombinationB iff (a1 > b1) OR (a1 = b1 AND a2 > b2) OR … (a1 = b1 AND a2 = b2 AND … ai = bi AND ai+1 > bi+1)
 The solution set must not contain duplicate combinations.
 Example,
 Given candidate set 2,3,6,7 and target 7,
 A solution set is:

 [2, 2, 3]
 [7]
*/


/**
Solution Outline:
	0. Let f(C, target, prefix) be a function that recursively computes if `target` can be achieved using elements from C.
		prefix : list of numbers so far that adds upto (actual_target - target)
	1. At each level, check if C[i] can be added to prefix, reducing the target needed by C[i]
		=> f(C, target-C[i], prefix+C[i])
		=> if target == 0, add prefix as a combination-sum.
	2. Backtrack to higher levels, if `target` sum cannot be achieved by current items in C.
		When backtracked into a level, remove the first item from C, and try f(C[1..], target, prefix)
	3. Initial call: f(C, actual_target, [])


Sample run:
	C: [2,3,4,7]
	target: 7

	L0: f([2,3,4,7], 7, [])
		L1: f([2,3,4,7], 5, [2])
			L2: f([2,3,4,7], 3, [2,2])
				L3: f([2,3,4,7], 1, [2,2,2])
					L4: f([2,3,4,7], -1, [2,2,2,2])
						<- Overshot target sum, cannot consider anything to the right as they will > as well [2,2,2,x], x: [3,4,7]
						<- backtrack to L3
				# remove 2 from candidates list
				L3: f([3,4,7], 1, [2,2,2])
					L4: f([3,4,7], -2, [2,2,2,3]) 
						<- Overshot target sum
						<- backtrack to L3
				L3: f([4,7],1, [2,2,2]) and f([7], 1, [2,2,2]) are dead-ends as well
					<- backtrack to L2
			L2: f([3,4,7], 3, [2,2])
				L3: f([3,4,7], 0, [2,2,3]) **** Combination: [2,2,3] = 7 ****
				L3: f([4,7], -1, [2,2,4])
					<- overshot sum,
					<- backtrack to L2
			L2: f([4,7], 3, [2,2])
				L3: f([4,7], -1, [2,2,4])
					<- overshot sum
					<- backtrack to L2
			L2: f([4,7], 3, [2,2])
				L3: f([7], -4, [2,2,7])
					<- overshot sum
					<- backtrack to L2
			L2: f([7], 3, [2,2])
				L3: f([7], -4, [2,2,7])
					<- overshot
					<- backtrack to L2
			L2: f([])
				<- no more candidates at L2 => All sums beginning with [2, ...] has been calculated
				<- backtrack to L1
		L1: f([3,4,7], 5, [2])
			L2: f([3,4,7], 2, [2,3])
				L3: f([3,4,7], -1, [2,3,3])
					<- backtrack to L2
			L2: f([4,7], 2, [2,3])
				L3: f([4,7], -3, [2,3,4])
					<- backtrack to L2
			L2: f([7], 2, ...) <- L1 => No combinations starting from [2,3..] using C adds upto 7
		L1: f([4,7], 5, [2])
			L2: f([4,7], 1, [2,4])
				L3: f([4,7], -3, [2,4,4]) <- dead-end, return to L2
			L2: f([7], 1, [2,4]) <- dead-end, return to L1
		L1: f([7], 5, [2]) <- dead-end, return to L0

	L0: f([3,4,7], 7, [])
		L1: f([3,4,7], 4, [3])
			L2: f([3,4,7], 1, [3,3]) <- dead-end
		L1: f([4,7], 4, [3])
			L2: f([4,7], 0, [3,4])   **** combination [3,4] = 7 ****
		L1: f([7], 4, [3]) <- dead-end

	L0: f([4,7], 7, [])
		L1: f([4,7], 3, [4])
			L2: f([4,7], -1, [4,4]) <- dead-end
		L1: f([7], 3, [4]) <- dead-end
						
	L0: f([7], 7, [])
		L1: f([7], 0, [7])  **** combination [7] = 7 ****
	 
	return { [2,2,3], [3,4], [7] }	
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
	if (!C.size())
		return;

	if (target < 0) {
		// backtrack to previous levels
		// the lowest number in C > required target sum
		return;
	}
	if (target == 0) {
		// target-sum can achieved with prefix+C[0]
		// add to results, and backtrack to previous levels
		// backtrack to previous levels
		// because current level can only
		// have 1 candidate to match the target sum
		VecInt result(prefix);
		results.insert(result);
		return;
	}

	for (auto i=0; i<C.size(); i++) {
		prefix.push_back(C[i]);
		combinations_sum(
				VecInt(C.begin()+i, C.end()),
				target-C[i],
				prefix,
				results);
		prefix.pop_back();
	}

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
		VecInt v = {3,2,6,7};
		assert( s.combinations_sum(v, 7) == (VecVecInt{{2,2,3}, {7}}) );
		assert( v == (VecInt{2,3,6,7}) );
	}

	{
		VecInt v = {8,10,6,11,16,8};
		assert( s.combinations_sum(v, 28) ==
				(VecVecInt{{6, 6, 6, 10}, {6, 6, 8, 8}, {6, 6, 16}, {6, 11, 11}, {8, 10, 10}}) );
	}
	{
		VecInt v{4,3,2,7};
		assert( s.combinations_sum(v, 7) == (VecVecInt{{2,2,3}, {3,4}, {7}}) );
	}
	{
		auto v = VecInt{2,3,6,7};
		assert( s.combinations_sum(v, 7) == (VecVecInt{{2,2,3}, {7}}) );
	}

	return 0;
}

