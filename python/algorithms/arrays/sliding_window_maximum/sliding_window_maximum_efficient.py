#encoding: utf-8

'''
https://leetcode.com/problems/sliding-window-maximum/

Given an array nums, there is a sliding window of size k which is moving from the very left of the array to the very right. You can only see the k numbers in the window. Each time the sliding window moves right by one position. Return the max sliding window.

Example:
Input: nums = [1,3,-1,-3,5,3,6,7], and k = 3
Output: [3,3,5,5,6,7] 
Explanation: 
Window position                Max
---------------               -----
[1  3  -1] -3  5  3  6  7       3
 1 [3  -1  -3] 5  3  6  7       3
 1  3 [-1  -3  5] 3  6  7       5
 1  3  -1 [-3  5  3] 6  7       5
 1  3  -1  -3 [5  3  6] 7       6
 1  3  -1  -3  5 [3  6  7]      7
Note: 
You may assume k is always valid, 1 ≤ k ≤ input array's size for non-empty array.

Follow up:
Could you solve it in linear time?
'''



'''
Solution Outline, O(n):
	0. Use a deque to keep track of maximums as we slide through the windows much like a queue with minimum implementation.
	1. Start with building the deque from from the first window of k items
	   For each item, i, remove all items to the right of the deque that are < i
	   e.g., window: [1,2,3], Deque: [1], then [2], and later [3], since 1, and 2 will be replaced by 3 as the maximum immediately.
	   Add item i to the right end of the deque.
	   if window was [4,3,2,1], deque at the end == [4,3,2,1]
	   Initialize max-list : [front of the deque]
	2. Once the initial deque has been created,
	   At each iteration,
	   Consider sliding to the next window as the equivalent of removing the first item in the current
	   window and adding a new one from the right end.
	   While 'removing' the first item, Check if the item just removed was the maximum (ie at the front of the deque).
	     If yes, remove it from the deque too.
	   Add new item at (i+k) after removing all items to the right of the deque which are lesser than it.
	   Add 'front of the deque' to the max-list as the maximum for the current window


Example 1:
=========
	nums: [4,3,2,1,2,3,4], k=3

	idx: 0 1 2 3 4 5 6
	[]:  4 3 2 1 2 3 4

	Initial window: [4,3,2]
	Deque: [4,3,2]
	max-list: [4]

	i: 3
	current window: [3,2,1]
	removing previous start of window = 4 = max, => remove from deque
	Deque: [3,2]
	Add new item to window = 1
	Deque: [3,2,1]
	max-list: [4,3]

	i:4
	current window: [2,1,2]
	removing previous start of window = 3 = max, => remove from deque
	Deque: [2,1]
	Add new item to window = 2, remove everything < 2, add 2
	Deque: [2,2]
	max-list: [4,3,2]

	i:5
	current window: [1,2,3]
	removing previous start of window = 2 = max, => remove from deque
	Deque: [2]
	Add new item to window = 3, remove everything < 3, add 3
	Deque: [3]
	max-list: [4,3,2,3]

	i:6
	current window: [2,3,4]
	removing previous start of window = 1 =/= max
	Deque: [3]
	Add new item to window = 4, remove everything < 4, add 4
	Deque: [4]
	max-list: [4,3,2,3,4]


Example 2:
=========
	nums = [1,3,-1,-3,5,3,6,7], k = 3

	idx: 0 1  2  3 4 5 6 7
	[]:  1 3 -1 -3 5 3 6 7

	Initial window: [1,3,-1]
	Deque: [3,-1]
	max-list: [3]

	i: 3
	current window: [3,-1,-3]
	removing previous start of window = 1 =/= max
	Deque: [3,-1]
	Add new item to window = -3
	Deque: [3,-1,-3]
	max-list: [3,3]

	i: 4
	current window: [-1,-3,5]
	removing previous start of window = 3 == max, remove from deque
	Deque: [-1,-3]
	Add new item to window = 5, remove everything < 5, then add 5
	Deque: [5]
	max-list: [3,3,5]

	i: 5
	current window: [-3,5,3]
	removing previous start of window = -1 =/= max
	Deque: [5]
	Add new item to window = 3, remove everything < 3, then add 3
	Deque: [5,3]
	max-list: [3,3,5,5]

	i: 6
	current window: [5,3,6]
	removing previous start of window = -3 =/= max
	Deque: [5,3]
	Add new item to window = 6, remove everything < 6, then add 6
	Deque: [6]
	max-list: [3,3,5,5,6]

	i:7
	current window: [3,6,7]
	removing previous start of window = 5 =/= max
	Deque: [6]
	Add new item to window = 7, remove everything < 7, then add 7
	Deque: [7]
	max-list: [3,3,5,5,6,7]
'''

from data_structures.dll.deque import Deque

def maxSlidingWindow(nums, k):
	"""
	:type nums: List[int]
	:type k: int
	:rtype: List[int]
	"""

	if not nums:
		return []

	dq = Deque()

	for i in xrange(k):
		# remove everything to te right of the deque
		# that is < current item
		# and then add current item
		while dq and dq.back() < nums[i]:
			dq.pop_back()
		dq.push_back(nums[i])
		
	max_list = [dq.front()]

	for i in xrange(k, len(nums)):
		# Previous SoW's maximum is still in the deque
		# and no longer needed
		if nums[i-k] == dq.front():
			dq.pop_front()

		# remove everything to te right of the deque
		# that is < current item
		# and then add current item
		while dq and dq.back() < nums[i]:
			dq.pop_back()
		dq.push_back(nums[i])

		# add current window's max from the top of the heap
		max_list.append(dq.front())

	return max_list


if __name__ == '__main__':
	assert maxSlidingWindow([5,1,4,3], 2) == [5,4,4]
	assert maxSlidingWindow([4,3,2,1], 2) == [4,3,2]
	assert maxSlidingWindow([4,3,2,1], 3) == [4,3]
	assert maxSlidingWindow([1,3,-1,-3,5,3,6,7], 3) == [3,3,5,5,6,7]
	assert maxSlidingWindow([9,10,9,-7,-4,-8,2,-6], 5) == [10, 10, 9, 2]
	assert maxSlidingWindow([1, -1], 1) == [1, -1]
	assert maxSlidingWindow([4,3,2,1,2,3,4], 3) == [4,3,2,3,4]
