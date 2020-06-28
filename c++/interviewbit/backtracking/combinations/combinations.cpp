/*
https://www.interviewbit.com/problems/combinations/

Combinations

Given two integers n and k, return all possible combinations of k numbers out of 1 2 3 ... n.

Make sure the combinations are sorted.

To elaborate,
Within every entry, elements should be sorted. [1, 4] is a valid entry while [4, 1] is not.
Entries should be sorted within themselves.
Example :
If n = 4 and k = 2, a solution is:

[
  [1,2],
  [1,3],
  [1,4],
  [2,3],
  [2,4],
  [3,4],
]

Warning : DO NOT USE LIBRARY FUNCTION FOR GENERATING COMBINATIONS.
Example : itertools.combinations in python.
If you do, we will disqualify your submission retroactively and give you penalty points. 
*/

/*
Solution Outline:
	1. Use DFS to generate all subsets upto length k, to get a lexicographically sorted list of subsets.
	2. Extract all subsets with length k.

Sample run:
	n = 6, k = 4

	prefix = []
	combinations = []

	dfs_combinations(0)
		prefix = [1]
		dfs_combinations(1)
			prefix = [1,2]
			dfs_combinations(2)
				prefix = [1,2,3]
				dfs_combinations(3)
					prefix = [1,2,3,4], == k
					combinations = [[1,2,3,4]]
				dfs_combinations(4)
					prefix = [1,2,3,5], == k
					combinations = [[1,2,3,4], [1,2,3,5]]
				dfs_combinations(5)
					prefix = [1,2,3,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6]]
			dfs_combinations(3)
				prefix = [1,2,4]
				dfs_combinations(4)
					prefix = [1,2,4,5], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5]]
				dfs_combinations(5)
					prefix = [1,2,4,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6]]
			dfs_combinations(4)
				prefix = [1,2,5]
				dfs_combinations(5)
					prefix = [1,2,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6]]
			dfs_combinations(5)
				prefix = [1,2,6]
		dfs_combinations(2)
			prefix = [1,3]
			dfs_combinations(3)
				prefix = [1,3,4]
				dfs_combinations(4)
					prefix = [1,3,4,5], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5]]
				dfs_combinations(5)
					prefix = [1,3,4,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6]]
			dfs_combinations(4)
				prefix = [1,3,5]
				dfs_combinations(5)
					prefix = [1,3,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6]]
			dfs_combinations(5)
				prefix = [1,3,6]
		dfs_combinations(3)
			prefix = [1,4]
			dfs_combinations(4)
				prefix = [1,4,5]
				dfs_combinations(5)
					prefix = [1,4,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6]]
			dfs_combinations(5)
				prefix = [1,4,6]
		dfs_combinations(4)
			prefix = [1,5]
			dfs_combinations(5)
				prefix = [1,5,6]
		dfs_combinations(5)
			prefix = [1,6]


	dfs_combinations(1)
		prefix = [2]
		dfs_combinations(2)
			prefix = [2,3]
			dfs_combinations(3)
				prefix = [2,3,4]
				dfs_combinations(4)
					prefix = [2,3,4,5], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6],
									[2,3,4,5]]
				dfs_combinations(5)
					prefix = [2,3,4,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6],
									[2,3,4,5], [2,3,4,6]]
			dfs_combinations(4)
				prefix = [2,3,5]
				dfs_combinations(5)
					prefix = [2,3,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6],
									[2,3,4,5], [2,3,4,6], [2,3,5,6]]
			dfs_combinations(5)
				prefix = [2,3,6]
		dfs_combinations(3)
			prefix = [2,4]
			dfs_combinations(4)
				prefix = [2,4,5]
				dfs_combinations(5)
					prefix = [2,4,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6],
									[2,3,4,5], [2,3,4,6], [2,3,5,6], [2,4,5,6]]
			dfs_combinations(5)
				prefix = [2,4,6]
		dfs_combinations(4)
			prefix = [2,5]
			dfs_combinations(5)
				prefix = [2,5,6]
		dfs_combinations(5)
			prefix = [2,6]

	dfs_combinations(2)
		prefix = [3]
		dfs_combinations(3)
			prefix = [3,4]
			dfs_combinations(4)
				prefix = [3,4,5]
				dfs_combinations(5)
					prefix = [3,4,5,6], == k
					combinations = [[1,2,3,4], [1,2,3,5], [1,2,3,6], [1,2,4,5], [1,2,4,6], [1,2,5,6], [1,3,4,5], [1,3,4,6], [1,3,5,6], [1,4,5,6],
									[2,3,4,5], [2,3,4,6], [2,3,5,6], [2,4,5,6],
									[3,4,5,6]]
			dfs_combinations(5)
				prefix = [3,4,6]
		dfs_combinations(4)
			prefix = [3,5]
			dfs_combinations(5)
				prefix = [3,5,6]
		dfs_combinations(5)
			prefix = [3,6]

	dfs_combinations(3,4,5) won't lead anywhere
*/
#include <vector>
#include <set>
#include <assert.h> 
#include <iostream>
using namespace std;
class Solution
{
	public:
		vector<vector<int>>
		enumerate_combinations(int n, int k)
		{
			vector<vector<int>> lexicographic_combinations;
			if (k<1 || k > n)
				return lexicographic_combinations;

			vector<int> prefix;
			for (int i=0; i<n-k+1; i++) {
				// enumerate all combinations starting from S[i]
				dfs_combinations(i, n, k, prefix, lexicographic_combinations);
			}

			return lexicographic_combinations;
		}

	private:
		/** Enumerate all combinations(subsets) beginning with (level+1) in lexicographic order */
		void
		dfs_combinations(int level,
				int n,
				int k,
				vector<int> &prefix,
				vector<vector<int>> &lexicographic_combinations)
		{
			// Add item at index 'level' to prefix
			prefix.push_back(level+1);
			if (prefix.size() == k) {
				// Add current subset only if its length == number of selections (k)
				lexicographic_combinations.push_back(prefix);
			} else {
				// prefix length < k
				for (int i=level+1; i<n; i++) {
					dfs_combinations(i, n, k, prefix, lexicographic_combinations);
				}
			}
			prefix.pop_back(); //backtrack
		}
};


int
main(void)
{
	Solution s;
	{
		vector<vector<int>> expected{};
		assert (s.enumerate_combinations(4,5) == expected);
	}
	{
		vector<vector<int>> expected{};
		assert (s.enumerate_combinations(5,0) == expected);
	}
	{
		vector<vector<int>> expected{{1,2,3,4,5}};
		assert (s.enumerate_combinations(5,5) == expected);
	}
	{
		vector<vector<int>> expected{{1}, {2}, {3}, {4}};
		assert (s.enumerate_combinations(4,1) == expected);
	}
	{
		vector<vector<int>> expected{
			{1,2}, {1,3}, {1,4},
			{2,3}, {2,4},
			{3,4}
		};
		assert (s.enumerate_combinations(4,2) == expected);
	}
	{
		vector<vector<int>> expected{
					{1,2,3,4}, {1,2,3,5}, {1,2,3,6}, {1,2,4,5}, {1,2,4,6}, {1,2,5,6}, {1,3,4,5}, {1,3,4,6}, {1,3,5,6}, {1,4,5,6},
				    {2,3,4,5}, {2,3,4,6}, {2,3,5,6}, {2,4,5,6},
					{3,4,5,6}
		};
		assert (s.enumerate_combinations(6,4) == expected);
	}
	return 0;
}


