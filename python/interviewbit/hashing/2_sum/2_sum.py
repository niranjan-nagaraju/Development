'''
https://www.interviewbit.com/problems/2-sum/

2 Sum

Given an array of integers, find two numbers such that they add up to a specific target number.

The function twoSum should return indices of the two numbers such that they add up to the target, where index1 < index2. Please note that your returned answers (both index1 and index2 ) are not zero-based.
Put both these numbers in order in an array and return the array from your function ( Looking at the function signature will make things clearer ). Note that, if no pair exists, return empty list.

If multiple solutions exist, output the one where index2 is minimum. If there are multiple solutions with the minimum index2, choose the one with minimum index1 out of them.

Input: [2, 7, 11, 15], target=9
Output: index1 = 1, index2 = 2
'''
class Solution:
	# @param A : tuple of integers
	# @param B : integer
	# @return a list of integers
	def twoSum(self, A, B):
		lookup = {}
		for i in xrange(len(A)):
			idx = lookup.get(B-A[i])
			if idx is None:
				# Couldn't find matching pair for A[i]
				# that adds upto B
				if lookup.get(A[i]) is None:
					# Store first-seen occurence of A[i]
					# so in case of conflicts, we'd return the
					# leftmost index for index1
					lookup[A[i]] = i
			else:
				return [idx+1, i+1]

		return []


if __name__ == '__main__':
	s = Solution()
	assert s.twoSum([2,7,11,15], 9) == [1,2]
	assert s.twoSum([7,11,2,15], 9) == [1,3]
	assert s.twoSum([4,7,2,2,2,-5,6], -3) == [3,6]

