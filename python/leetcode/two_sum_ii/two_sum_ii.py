'''
https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/

167. Two Sum II - Input array is sorted

Given an array of integers that is already sorted in ascending order, find two numbers such that they add up to a specific target number.

The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2.

Note:
Your returned answers (both index1 and index2) are not zero-based.
You may assume that each input would have exactly one solution and you may not use the same element twice.
Example:

Input: numbers = [2,7,11,15], target = 9
Output: [1,2]
Explanation: The sum of 2 and 7 is 9. Therefore index1 = 1, index2 = 2.
'''

class Solution(object):
	def twoSum(self, numbers, target):
		"""
		:type numbers: List[int]
		:type target: int
		:rtype: List[int]
		"""
		i, j = 0, len(numbers)-1
		while i<j:
			if target == numbers[i] + numbers[j]:
				return (i+1,j+1)  # results are 1-indexed
			elif target > numbers[i] + numbers[j]:
				# Move left window so sum increases
				i += 1
			else: # target < numbers[i] + numbers[j]:
				# Move right window so sum decreases
				j -= 1

		# couldn't find target sum
		return -1, -1
        

if __name__ == '__main__':
	s = Solution()
	assert s.twoSum([2,7,11,15], 9) == (1,2)
	assert s.twoSum([1,2,3,4,5], 7) == (2,5)

