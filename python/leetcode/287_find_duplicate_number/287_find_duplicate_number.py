'''
https://leetcode.com/problems/find-the-duplicate-number/

287. Find the Duplicate Number

Given an array of integers nums containing n + 1 integers where each integer is in the range [1, n] inclusive.
There is only one repeated number in nums, return this repeated number.
You must solve the problem without modifying the array nums and uses only constant extra space.

Example 1:
Input: nums = [1,3,4,2,2]
Output: 2

Example 2:
Input: nums = [3,1,3,4,2]
Output: 3

Constraints:
    1 <= n <= 105
    nums.length == n + 1
    1 <= nums[i] <= n
    All the integers in nums appear only once except for precisely one integer which appears two or more times.


Follow up:
    How can we prove that at least one duplicate number must exist in nums?
    Can you solve the problem in linear runtime complexity?
'''

'''
Solution Outline:
0. Consider each index to be a node in an SLL, and the contents to be the next link.
1. The problem then boils down to finding the starting point of a loop/cycle in an SLL.

Example 1:
0 1 2 3 4
1 3 4 2 2

nums[0] = 1 === node0 -> node1
nums[1] = 3 === node1 -> node3
nums[2] = 4 === node2 -> node4
nums[3] = 2 === node3 -> node2
nums[4] = 2 === node4 -> node2

node0 -> node1 -> node3 -> node2 -> node4 ->|
                             ^ -------------|


Example 2:
0 1 2 3 4
3 1 3 4 2

nums[0] = 3 === node0 -> node3
nums[1] = 1 === node1 -> node1
nums[2] = 3 === node2 -> node3
nums[3] = 4 === node3 -> node4
nums[4] = 2 === node4 -> node2

node0 -> node3 -> node4 -> node2 ->|
           ^-----------------------|

all numbers are in [1..n] while there are n+1 indices => [0..n] indices are valid
nothing points to 0 since nums[x] can be 0 for any x
'''

class Solution:
	def findDuplicate(self, nums):
		"""
		:type nums: List[int]
		:rtype: int
		"""
		slow = nums[0] 
		fast = nums[nums[0]]
		while slow != fast:
			slow = nums[slow]
			fast = nums[nums[fast]]

		# At this point, fast and slow pointer are at the same node,
		# 'k' nodes away from the front of the SLL to the start of the cycle
		# Since both front of the SLL and 'fast' are equidistant to the cycle-start,
		# Reset 'slow' to the front of the SLL and continue 'fast' within the loop (but this time at the same pace)
		# When 'slow' and 'fast' meet, they are at the start of the cycle
		slow = 0
		while slow != fast:
			slow = nums[slow]
			fast = nums[fast]

		return slow

if __name__ == '__main__':
	s = Solution()
	assert s.findDuplicate([1,3,4,2,2]) == 2
	assert s.findDuplicate([3,1,3,4,2]) == 3
	
