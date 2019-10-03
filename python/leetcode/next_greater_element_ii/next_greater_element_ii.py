'''
https://leetcode.com/problems/next-greater-element-ii/

503. Next Greater Element II

Given a circular array (the next element of the last element is the first element of the array), print the Next Greater Number for every element. The Next Greater Number of a number x is the first greater number to its traversing-order next in the array, which means you could search circularly to find its next greater number. If it doesn't exist, output -1 for this number.

Example 1:
Input: [1,2,1]
Output: [2,-1,2]
Explanation: The first 1's next greater number is 2; 
The number 2 can't find next greater number; 
The second 1's next greater number needs to search circularly, which is also 2.
Note: The length of given array won't exceed 10000.
'''

'''
Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack and solve the problem of next-greater-element like matching parantheses.
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack
	4. Since its a circular array, traverse the array again (after reaching the end), until the penultimate item.
       a = [1,2,3,4]
	   1,2,3,4,1,2,3 covers the circular array.
	5. Fill nge[x] only if it has not been found yet.
	   This ensures that a previously found nge[] is  not overwritten by a new one in the circular traversal,
	   as the new one is certainly 'farther'.
		e.g. [5,1,2,3,4]
	    nge[1] should not be overwritten by 5, and should remain 2
'''
class Solution(object):
	def nextGreaterElements(self, nums):
		"""
		:type nums: List[int]
		:rtype: List[int]
		"""
		stack = []
		nge = [-1] * len(nums)

		for i in xrange(len(nums)):
			while stack and nums[stack[0]] < nums[i]:
				x = stack.pop(0)
				nge[x] = nums[i]
			stack.insert(0, i)

		# Circular traversal, traverse till the penultimate item this time
		for i in xrange(len(nums)-1):
			while stack and nums[stack[0]] < nums[i]:
				x = stack.pop(0)
				# if NGE has already been found for the current item
				# Ignore it for this pass
				if nge[x] == -1:
					nge[x] = nums[i]
			stack.insert(0, i)

		return nge


if __name__ == '__main__':
	s = Solution()
	assert s.nextGreaterElements([1,2,1]) == [2,-1,2]
	assert s.nextGreaterElements([1,2,3,4]) == [2,3,4,-1]
	assert s.nextGreaterElements([3,1,2,4]) == [4,2,4,-1]
	assert s.nextGreaterElements([4,5,2,25]) == [5,25,25,-1]
	assert s.nextGreaterElements([13,7,6,12]) == [-1,12,12,13]
	assert s.nextGreaterElements([4,3,2,1]) == [-1,4,4,4]

