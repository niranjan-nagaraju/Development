'''
https://leetcode.com/problems/max-consecutive-ones-iii/

Given an array A of 0s and 1s, we may change up to K values from 0 to 1.

Return the length of the longest (contiguous) subarray that contains only 1s. 

 Example 1:
 Input: A = [1,1,1,0,0,0,1,1,1,1,0], K = 2
 Output: 6
 Explanation: 
 [1,1,1,0,0,1,1,1,1,1,1]
 Bolded numbers were flipped from 0 to 1.  The longest subarray is underlined.

 Example 2:
 Input: A = [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], K = 3
 Output: 10
 Explanation: 
 [0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1]
 Bolded numbers were flipped from 0 to 1.  The longest subarray is underlined.
'''


class Solution(object):
	def longestOnes(self, A, K):
		"""
		:type A: List[int]
		:type K: int
		:rtype: int
		"""
		zeroes = self.filter_0s(A)
		longest_1s = 0
		for i in range(len(zeroes)):
			flipped = self.flip_k_0s(zeroes[:], i, K)
			curr_longest = self.local_longest_1s(flipped, len(A))
			if  curr_longest > longest_1s:
				longest_1s = curr_longest

		return longest_1s


	'''
	Given an array which contains indices where the 0s are, in an array of 0s and 1s
	return the longest interval containing only 1s.

	e.g.
	n: 10
	0s: [3,4,5]
	=> Array: [1,1,1,0,0,0,1,1,1,1]
	=> longest 1s sequence is 4
	'''
	@staticmethod
	def local_longest_1s(zeroes, n):
		if zeroes == []:
			return n
		longest_1s = zeroes[0]
		for i in xrange(1, len(zeroes)):
			curr_seq_length = zeroes[i] - zeroes[i-1] - 1
			if curr_seq_length > longest_1s:
				longest_1s = curr_seq_length

		if (n-zeroes[-1]-1) > longest_1s:
			longest_1s = (n-zeroes[-1]-1)

		return longest_1s


	'''
	Given an array of 1s and 0s, filter indices which contains 0s
	and return the list of indices
	'''
	@staticmethod
	def filter_0s(array):
		zeroes = []
		for i in xrange(len(array)):
			if array[i] == 0:
				zeroes.append(i)

		return zeroes



	'''
	In a zeroes list that contains indices of 0s (from the original list),
	remove k items from 'start'
	removing an index from the zeroes list => that index has been flipped to 1 in the original list
	'''
	@staticmethod
	def flip_k_0s(zeroes, start, k):
		if start + k > len(zeroes):
			return zeroes

		for _ in range(k):
			zeroes.pop(start)

		return zeroes





if __name__ == '__main__':
	sol = Solution()
	assert(sol.filter_0s([1, 0, 1, 1, 1, 1, 1, 1, 1, 0]) == [1,9])
	assert(sol.filter_0s([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1]) == [0, 1, 4, 5, 9, 12, 13, 14])
	assert(sol.filter_0s([1,1,1,0,0,0,1,1,1,1,0]) == [3,4,5,10])

	assert(sol.flip_k_0s([1,9], 0, 1) == [9])
	assert(sol.flip_k_0s([1,9], 1, 2) == [1,9])
	assert(sol.flip_k_0s([0, 1, 4, 5, 9, 12, 13, 14], 0, 2) == [4, 5, 9, 12, 13, 14])
	assert(sol.flip_k_0s([0, 1, 4, 5, 9, 12, 13, 14], 2, 2) == [0, 1, 9, 12, 13, 14])
	assert(sol.flip_k_0s([0, 1, 4, 5, 9, 12, 13, 14], 4, 2) == [0, 1, 4, 5,  13, 14])
	assert(sol.flip_k_0s([0, 1, 4, 5, 9, 12, 13, 14], 6, 2) == [0, 1, 4, 5,   9, 12])
	assert(sol.flip_k_0s([0, 1, 4, 5, 9, 12, 13, 14], 8, 2) == [0, 1, 4, 5, 9, 12, 13, 14])

	assert(sol.local_longest_1s([3,4,5], 10) == 4)
	assert(sol.local_longest_1s([], 10) == 10)
	assert(sol.local_longest_1s([1, 9], 10) == 7)  # [1, 0, 1, 1, 1, 1, 1, 1, 1, 0]
	assert(sol.local_longest_1s([0,1,4,5,9,12,13,14], 19) == 4)  # [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1]

	assert sol.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 3) == 10
	assert sol.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 2) == 7
	assert sol.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 2) == 6
	assert sol.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 3) == 10


