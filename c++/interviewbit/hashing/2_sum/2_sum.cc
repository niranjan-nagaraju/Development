/**
https://www.interviewbit.com/problems/2-sum/

2 Sum

Given an array of integers, find two numbers such that they add up to a specific target number.

The function twoSum should return indices of the two numbers such that they add up to the target, where index1 < index2. Please note that your returned answers (both index1 and index2 ) are not zero-based.
Put both these numbers in order in an array and return the array from your function ( Looking at the function signature will make things clearer ). Note that, if no pair exists, return empty list.

If multiple solutions exist, output the one where index2 is minimum. If there are multiple solutions with the minimum index2, choose the one with minimum index1 out of them.

Input: [2, 7, 11, 15], target=9
Output: index1 = 1, index2 = 2
*/

#include <unordered_map>
#include <vector>
#include <cassert>

class Solution
{
public:
	std::vector<int> twoSum(const std::vector<int> &A, int B) {
		std::vector<int> res;
		std::unordered_map<int, int> lookup;

		for (int i=0; i<A.size(); i++) {
			auto it = lookup.find(B-A[i]);
			if (it == lookup.end()) {
				// Couldn't find matching pair for A[i]
				// that adds upto B
				if (lookup.find(A[i]) == lookup.end()) {
					// Store first-seen occurence of A[i]
					// so in case of conflicts, we'd return the
					// leftmost index for index1
					lookup[A[i]] = i;
				}
			} else {
				// Found a pair that adds upto B
				res.push_back((it->second)+1);
				res.push_back(i+1);
				break;
			}
		}

		return res;
	}
};



int main(void)
{
	Solution s;
	{
		std::vector<int> in = {2,7,11,15};
		std::vector<int> out = {1,2};
		assert(s.twoSum(in, 9) == out);
	}
	{
		std::vector<int> in = {7,11,2,15};
		std::vector<int> out = {1,3};
		assert(s.twoSum(in, 9) == out);
	}
	{
		std::vector<int> in = {4, 7, 2, 2, 2, -5, 6};
		std::vector<int> out = {3, 6};
		assert(s.twoSum(in, -3) == out);
	}

	return 0;
}
