#encoding: utf-8
'''
https://www.hackerrank.com/challenges/max-array-sum/problem

Max Array Sum

Given an array of integers, find the subset of non-adjacent elements with the maximum sum. Calculate the sum of that subset.
For example, given an array  arr = [-2,1,3,-4,5], we have the following possible subsets:

	Subset      Sum
	[-2, 3, 5]   6
	[-2, 3]      1
	[-2, -4]    -6
	[-2, 5]      3
	[1, -4]     -3
	[1, 5]       6
	[3, 5]       8
Our maximum subset sum is 8.

Input Format
The first line contains an integer, n.
The second line contains  space-separated integers arr[i].

Output Format
Return the maximum sum described in the statement.

Sample Input 0
5
3 7 4 6 5
Sample Output 0
13
Explanation 0
Our possible subsets are
[3, 4, 5] : 12
[3, 4] : 7
[3, 6] : 9
[3. 5] : 8
[7, 6] : 13
[7, 5] : 12
[4, 5] : 9

Sample Input 1
5
2 1 5 8 4
Sample Output 1
11
Explanation 1
Possible subsets
[2, 5, 4]: 11
[2, 5] : 7
[2, 8] : 10
[2, 4] : 6
[1, 8] : 9
[1, 4] : 5
[5, 4] : 9

Sample Input 2
5
3 5 -7 8 10
Sample Output 2
15
Explanation 2
Possible subsets
[3, -7, 10] : 6
[3, -7] : -4
[3, 8] : 11
[3, 10] : 13
[5, 8] : 13
[5, 10] : 15
[-7, 10] : 3
'''


'''
Generate all subsets of a list  where none of the elements in the subset
are adjacent to each other in the original list

'''

'''
Solution Outline:  Brute-force O(2ⁿ)
  1. Generate all non-adjacent subsets
	 E.g.,
	 A: [1,2,3,4,5]
	 subsets:
	   [1]
	   [1,3]
	   [1,3,5]
	   [1,4]
	   [1,5]
	   [2]
	   [2,4]
	   [2,5]
	   [3]
	   [3,5]
	   [4]
	   [5]

  2. Compute their sums, and return the maximum amongst them
'''
def max_non_adjacent_subset_sum(a):
	def non_adjacent_subsets_(subs_idxs):
		x = subs_idxs[-1]
		if x >= len(a):
			return

		# Fill a[x] from each index in subset indexes
		# and ompute its sum
		subset_sum = sum((map(lambda x: a[x], subs_idxs)))
		if subset_sum > max_sum[0]:
			max_sum[0] = subset_sum

		# Add index+2 elements
		subs_idxs.append(x+2)
		non_adjacent_subsets_(subs_idxs)
		subs_idxs.pop() # backtrack

		# Once we are done with index+2, replace last element in the subset with index+1
		# so we are still handling the same length subset at this level
		subs_idxs[-1] = x+1
		non_adjacent_subsets_(subs_idxs)

	max_sum = [a[0]]
	non_adjacent_subsets_([0])
	return max_sum[0]


if __name__ == '__main__':
	assert max_non_adjacent_subset_sum([-2, 1, 3, -4, 5]) == 8
	assert max_non_adjacent_subset_sum([3, 7, 4, 6, 5]) == 13
	assert max_non_adjacent_subset_sum([2, 1, 5, 8, 4]) == 11 
	assert max_non_adjacent_subset_sum([3, 5, -7, 8, 10]) == 15
	assert max_non_adjacent_subset_sum([3, 10, 5, -8, 13]) == 23
	assert max_non_adjacent_subset_sum([5, 5, 10, 100, 10, 5]) == 110
	assert max_non_adjacent_subset_sum([1,2,3]) == 4
	assert max_non_adjacent_subset_sum([1,20,3]) == 20
	assert max_non_adjacent_subset_sum([1,2,3,4,5]) == 9

