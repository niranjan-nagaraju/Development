/*
https://www.interviewbit.com/problems/subset/

Subset

Given a set of distinct integers, S, return all possible subsets.

Note:
	Elements in a subset must be in non-descending order.
	The solution set must not contain duplicate subsets.
	Also, the subsets should be sorted in ascending ( lexicographic ) order.
	The list is not necessarily sorted.

Example :
If S = [1,2,3], a solution is:

[
  [],
  [1],
  [1, 2],
  [1, 2, 3],
  [1, 3],
  [2],
  [2, 3],
  [3],
]
*/


/*
Solution Outline:
	1. Sort the input, S,  to get a lexicographically sorted list
	2. Add [] empty subset to the powerset
	3. Start a DFS traversal beginning at level=0, with neighbors of level, i, as (i+1 .. n-1) {n: len(S)}
	   Each DFS call is sent a prefix of all the elements in this current path.
	   Backtrack by unwinding elements at each level after DFS for that level returns
	   At the end of the DFS traversal, all subsets beginning with S[0] will be generated.
	4. Repeat {step 2}. with level = 1, 2, 3, .. n-1 to get all the subsets (in lexicographically sorted order)

Sample run:
	S: [2,1,3]
	
	sort: S: [1,2,3]
	subsets = []

	DFS traversal(level=0)
	  prefix: [1], Add to subsets
	   subsets: []
				[1]
		DFS traversal(level=1)
		prefix: [1,2], Add to subsets
		 subsets: []
				  [1], [1,2]
			DFS traversal(level=2)
			prefix: [1,2,3], Add to subsets
			 subsets: []
					  [1], [1,2], [1,2,3]
		DFS traversal(level=1)
		prefix: [1,3], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,3], [1,3]

	DFS traversal(level=1)
	  prefix: [2], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,3], [1,3]
				[2]
		DFS traversal(level=2)
		prefix: [2,3], Add to subsets
		 subsets: []
				  [1], [1,2], [1,2,3], [1,3]
				  [2], [2,3]

	DFS traversal(level=2)
	  prefix: [3], Add to subsets
	   subsets: []
				[1], [1,2], [1,2,3], [1,3]
				[2], [2,3]
				[3]
*/


#include<vector>
#include<set>
#include<assert.h> 
#include <iostream>
using namespace std;
class Solution
{
	public:
		/** Enumerate all subsets of S in lexicographic order */
		vector<vector<int>>
		enumerate_all_subsets(vector<int> &S)
		{
			vector<vector<int>> lexicographic_subsets;
			sort(S.begin(), S.end());

			vector<int> prefix;

			// Initialize with empty set
			lexicographic_subsets.push_back(prefix);

			for (int i=0; i<S.size(); i++) {
				// enumerate all subsets starting from S[i]
				dfs_subsets(i, S, prefix, lexicographic_subsets);
			}

			return lexicographic_subsets;
		}

	private:
		/** Enumerate all subsets beginning with S[level] in lexicographic order */
		void
		dfs_subsets(int level,
				vector<int> &S,
				vector<int> &prefix,
				vector<vector<int>> &lexicographic_subsets)
		{
			prefix.push_back(S[level]);
			lexicographic_subsets.push_back(prefix);

			for (int i=level+1; i<S.size(); i++) {
				dfs_subsets(i, S, prefix, lexicographic_subsets);
			}
			prefix.pop_back(); //backtrack
		}

};


int
main(void)
{

	Solution s;
	{
		vector<int>A = {};
		vector<vector<int>> expected{{}};

		assert (s.enumerate_all_subsets(A) == expected);
	}
	{
		vector<int>A = {1};
		vector<vector<int>> expected{{}, {1}};

		assert (s.enumerate_all_subsets(A) == expected);
	}
	{
		vector<int>A = {1,2};
		vector<vector<int>> expected{{}, {1}, {1,2}, {2}};

		assert (s.enumerate_all_subsets(A) == expected);
	}
	{
		vector<int>A = {2,1,3};
		vector<vector<int>> expected{
										{},
										{1}, {1,2}, {1,2,3}, {1,3},
										{2}, {2,3},
										{3}
									};
		assert (s.enumerate_all_subsets(A) == expected);
	}
	{
		vector<int>A = {2,4,1,3};
		vector<vector<int>> expected{
										{},
										{1}, {1,2}, {1,2,3}, {1,2,3,4}, {1,2,4}, {1,3}, {1,3,4}, {1,4},
										{2}, {2,3}, {2,3,4}, {2,4},
										{3}, {3,4},
										{4}
									};
		assert (s.enumerate_all_subsets(A) == expected);
	}

	return 0;
}


