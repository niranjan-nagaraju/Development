/*
https://www.interviewbit.com/problems/3-sum-zero/

3 Sum Zero

Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0?
Find all unique triplets in the array which gives the sum of zero.

Note:
	Elements in a triplet (a,b,c) must be in non-descending order. (ie, a ≤ b ≤ c)
	The solution set must not contain duplicate triplets.
	For example, given array S = {-1 0 1 2 -1 -4}, A solution set is:
	(-1, 0, 1)
	(-1, -1, 2) 
*/

/*
Solution Outline:
	1. Sort the input array S
	2. Start with a,b,c = S[i], S[i+1], and S[n-1], i = 0
	   2.1 Calculate curr_sum := (a+b+c)
	   2.2 If curr_sum is greater than 0, Move c towards left to reduce curr_sum
		   If curr_sum is lesser than 0, Move b towards right to increase curr_sum
		   If curr_sum == 0 => Add (a,b,c) to a set
	   2.4 If b & c cross each other, Start 2. over with i=1
		     a,b,c = S[i], S[i+1], S[-1]
	3. return all the unique triplets accumulated in the set as a list of list
*/

#include<vector>
#include<set>
#include<assert.h> 
using namespace std;

class Solution {
	public:
		vector<vector<int>> find_3sum_zero(vector<int> &A);

};


vector<vector<int>>
Solution::find_3sum_zero(vector<int> &A)
{
	vector<vector<int>> triplets;
	set<vector<int>> triplets_set;
	    
    if (A.size() < 3)
        return triplets;

	sort(A.begin(), A.end());
	for (int i=0; i < A.size()-2; i++) {
		int l = i+1;
		int r = A.size()-1;

		while (l < r) {
			/** incase sum overflows 32-bits */
			long long curr_sum = 0LL + A[i] + A[l] + A[r];

			if (curr_sum == 0) {
				// Found a matching triplet adding to 0
				vector<int> triplet = {A[i], A[l], A[r]};
				triplets_set.insert(triplet);
				l++; r--;
			} else if (curr_sum < 0) {
				// increase sum, move l to the right
				l += 1;
			} else { //curr_sum > 0
				// decrease sum, move r to the left
				r -= 1;
			}
		}
	}

	for (auto v : triplets_set) {
		triplets.push_back(v);
	}
	return triplets;
}


int
main(void)
{
	Solution s;
	{
		vector<int>v = {-1,0,1,2,-1,-4};
		vector<vector<int>> expected{{-1,-1,2},{-1,0,1}};
		assert (s.find_3sum_zero(v) == expected);
	}
	{
		vector<int>v = {1,2,3,4,-5};
		vector<vector<int>> expected{{-5,1,4},{-5,2,3}};
		assert (s.find_3sum_zero(v) == expected);
	}
	{
		vector<int>v = {1,2,3,4,5};
		vector<vector<int>> expected{};
		assert (s.find_3sum_zero(v).size() == 0);
		assert(s.find_3sum_zero(v) == expected);
	}
	return 0;
}

