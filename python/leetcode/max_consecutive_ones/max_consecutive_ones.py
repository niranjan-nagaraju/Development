'''
https://leetcode.com/problems/max-consecutive-ones/

485. Max Consecutive Ones
Given a binary array, find the maximum number of consecutive 1s in this array.

Example 1:
Input: [1,1,0,1,1,1]
Output: 3
Explanation: The first two digits or the last three digits are consecutive 1s.
    The maximum number of consecutive 1s is 3.

Note:
The input array will only contain 0 and 1.
The length of input array is a positive integer and will not exceed 10,000
'''

class Solution(object):
	def findMaxConsecutiveOnes(self, nums):
		"""
		:type nums: List[int]
		:rtype: int
		"""
		count = 0
		max_count = 0
		for x in nums:
			count = 0 if (x==0) else (count+1)
			max_count = max(count, max_count)
		return max_count

if __name__ == '__main__':
	s = Solution()
	assert s.findMaxConsecutiveOnes([1,1,0,1,1,1]) == 3
	assert s.findMaxConsecutiveOnes([1,0,1,1,0,1]) == 2

