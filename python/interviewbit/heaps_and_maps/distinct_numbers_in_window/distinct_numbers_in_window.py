'''
https://www.interviewbit.com/problems/distinct-numbers-in-window/

Distinct Numbers in Window

Problem Description
You are given an array of N integers, A1, A2 ,..., AN and an integer B. Return the of count of distinct numbers in all windows of size B.
Formally, return an array of size N-B+1 where i'th element in this array contains number of distinct elements in sequence Ai, Ai+1 ,..., Ai+B-1.
NOTE: if B > N, return an empty array.

Input Format
First argument is an integer array A
Second argument is an integer B.

Output Format
Return an integer array.

Example Input
Input 1:
 A = [1, 2, 1, 3, 4, 3]
 B = 3
Input 2:
 A = [1, 1, 2, 2]
 B = 1

Example Output
Output 1:
 [2, 3, 3, 2]
Output 2:
 [1, 1, 1, 1]

Example Explanation
Explanation 1:
 A=[1, 2, 1, 3, 4, 3] and B = 3
 All windows of size B are
 [1, 2, 1]
 [2, 1, 3]
 [1, 3, 4]
 [3, 4, 3]
 So, we return an array [2, 3, 3, 2].
Explanation 2:
 Window size is 1, so the output array is [1, 1, 1, 1].
'''


'''
Solution Outline:
	1. Use a hash-table to store frequencies of current window.
	2. Initialize the hash-table with the starting window of k-elements.
		Elements which aren't unique go to the same bucket and will be counted only once.
	3. For every element after initial window, slide the window 1-element at a time to the right,
		3.1 Decrement frequency for the previous start-of-window,
			if its frequency is now 0 => number of distinct elements in the window reduced by 1
		3.2 Increment frequency for the new end-of-window,
			if its frequency is now 1 => number of distinct elements in the window increased by 1


Sample run:
	A: [1,2,1,3,4,3]
	k: 3
	Distinct: []

	frequencies: {}
	
	window: [1,2,1]
	frequencies:
		{
			1: 2
			2: 1
		}
	num_distinct = 2
	Distinct: [2]

	slide window: [2,1,3]
	previous sow: 1
	frequencies:
		{
			1: 1
			2: 1
		}
	num_distinct: 2
	new eow: 3
		frequencies:
		{
			1: 1
			2: 1
			3: 1
		}
	num_distinct: 3
	Distinct: [2, 3]

	slide window: [1,3,4]
	previous sow: 2
	frequencies:
		{
			1: 1
			2: 0
			3: 1
		}
	num_distinct: 2
	new eow: 4
		frequencies:
		{
			1: 1
			2: 0
			3: 1
			4: 1
		}
	num_distinct: 3
	Distinct: [2, 3, 3]

	slide window: [3,4,3]
	previous sow: 1
	frequencies:
		{
			1: 0
			2: 0
			3: 1
			4: 1
		}
	num_distinct: 2
	new eow: 3
		frequencies:
		{
			1: 0
			2: 0
			3: 2
			4: 1
		}
	num_distinct: 2
	Distinct: [2, 3, 3, 2]
return [2,3,3,2]
'''
class Solution:
	def count_distinct_numbers_in_window(self, A, k):
		if k == 0 or k > len(A):
			return []

		if k == 1:
			return [1]*len(A)


		from collections import defaultdict
		frequencies = defaultdict(lambda: 0)

		# Initial window
		for i in xrange(k):
			frequencies[A[i]] += 1

		# Initial window's distinct numbers count is the number of
		# elements in the frequencies table
		num_distinct = len(frequencies)
		window_distinct_counts = [num_distinct]

		for i in xrange(k, len(A)):
			prev_sow = A[i-k]
			new_eow = A[i]

			frequencies[prev_sow]-= 1
			if frequencies[prev_sow] == 0:
				# previous-sow value no longer exists in current window
				num_distinct -= 1

			if frequencies[new_eow] == 0:
				# A[i] is new to the current window
				num_distinct += 1
			frequencies[new_eow]+= 1

			window_distinct_counts.append(num_distinct)

		return window_distinct_counts


if __name__ == '__main__':
	s = Solution()
	assert s.count_distinct_numbers_in_window([1,2,1,3,4,3], 3) == [2,3,3,2]
	assert s.count_distinct_numbers_in_window([1,2,1,3,4,3], 2) == [2,2,2,2,2]
	assert s.count_distinct_numbers_in_window([1,1,2,3,3,4], 2) == [1,2,2,1,2]
	assert s.count_distinct_numbers_in_window([1,1,2,3,3,4], 1) == [1,1,1,1,1,1]

