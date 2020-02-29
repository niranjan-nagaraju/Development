'''
https://www.interviewbit.com/problems/first-missing-integer/

First Missing Integer


Given an unsorted integer array, find the first missing positive integer.

Example:

Given [1,2,0] return 3,

[3,4,-1,1] return 2,

[-8, -7, -6] returns 1

Your algorithm should run in O(n) time and use constant space.
'''

'''
Solution Outline: O(n) time, O(1) memory
	Consider an array, A, with only +ve integers.
	A way to remember if a +ve integer, x, is in the array A is by flipping A[x] to -A[x] (assuming 1 <= x <= n-1)
	Scan the array, registering if x is present in the array if 1 <= x <= n-1.
	Scan 1-N, find the first missing integer.
	NOTE: If the array A contains -ve and 0s as well, Bubble them to the end of the array
	      and consider only the subarray of positive integers.
	NOTE: In reality, Track x by A[x-1] = -A[x-1],
	      so if A has all [1..n], we'll know to return (n+1)

Sample run 1:
	A: [-4, 7, 1, 2, 3, 5, 4, 2, -3]

	Preprocess:
	  Bubble [-4, -3] to the end of the list
	  A: [2, 7, 1, 2, 3, 5, 4, -4, -3]
	  Truncate -ve numbers
	  A: [2, 7, 1, 2, 3, 5, 4]
          0  1  2  3  4  5  6
	  i: 0, |x| = 2
		A[1] = -|A[1]| = -7
	    A: [2, -7, 1, 2, 3, 5, 4]

	  i: 1, |x| = 7
		A[6] = -[A[6]| = -4
	    A: [2, -7, 1, 2, 3, 5, -4]

	  i: 2, |x| = 1
	    A[0] = -|A[0]| = -2
	    A: [-2, -7, 1, 2, 3, 5, -4]

	  i: 3, |x| = 2
	    A[1] = -|A[1]| = -7
	    A: [-2, -7, 1, 2, 3, 5, -4]

	  i: 4, |x| = 3
	    A[2] = -|A[2]| = -1
	    A: [-2, -7, -1, 2, 3, 5, -4]

	  i: 5, |x| = 5
	    A[4] = -|A[4]| = -3
	    A: [-2, -7, -1, 2, -3, 5, -4]

	  i: 6, |x| = 4
	    A[3] = -|A[3]| = -2
	    A: [-2, -7, -1, -2, -3, 5, -4]
            0   1   2   3   4   5   6 
	 Find missing number fromm [1-7]
	 A[0] is -ve: 1 is in A
	 A[1] is -ve
	 A[2] is -ve
	 A[3] is -ve
	 A[4] is -ve
	 A[5] is +ve => 6 is missing

	 return 6
'''
class Solution:
	# Partition positive integers to the left,
	# non-positive ones to the right
	def partition(self, a):
		i, j = 0, len(a)-1
		while i <= j:
			if a[i] > 0:
				i += 1
			elif a[j] <= 0:
				j -= 1
			else:
				a[i], a[j] = a[j], a[i]
				i += 1
				j -= 1

		# i -> number of +ve numbers
		return  i


	# @param a : list of integers
	# @return an integer
	def firstMissingPositive(self, a):
		n = len(a)

		# Truncate non +ve numbers
		n = self.partition(a)

		for i in xrange(n):
			x = abs(a[i])
			if x <= n:
				# Flip a[x-1]'s sign to hint that 'x' is in the array
				a[x-1] = -abs(a[x-1])

		# Look for missing number between [1..n]
		for i in xrange(n):
			if a[i] > 0:
				return i+1

		# All [1..n] are found in the list
		# => (n+1) is the first +ve integer missing
		return (n+1)


if __name__ == '__main__':
	s = Solution()
	assert s.firstMissingPositive([1,2,0]) == 3
	assert s.firstMissingPositive([3,4,-1,1]) == 2
	assert s.firstMissingPositive([-8, -7, -6]) == 1
	assert s.firstMissingPositive([1,2,3,4,5]) == 6
	assert s.firstMissingPositive([1,3,2,5,0,4]) == 6
	assert s.firstMissingPositive([1,5,2,4]) == 3
	assert s.firstMissingPositive([6,0,1,4,3,2]) == 5
	assert s.firstMissingPositive([-4, 7, 1, 2, 3, 5, 4, 2, -3]) == 6


