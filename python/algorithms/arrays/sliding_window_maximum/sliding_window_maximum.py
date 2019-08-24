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
Solution Outline, O(nlogn):
	0. Use a heap to repeatedly get the maximum in the current window
	1. Start with building a max-heap from from the first window of k items
	2. When the current top of the heap, or the current window's maximum goes out of window,
	   remove it from the heap, adjust heap, add new item, adjust heap so current top of the heap
	   contains the new window's maximum.

Example:
	nums = [1,3,-1,-3,5,3,6,7], k = 3
	initial, i: 0 -> k-1(2)
	heap: [(3,1), (1,0) (-1,2)]
          (3,1)
        /       \
      (1,0)	   (-1,2)
	max: [3]

	i:3, window [1..3]
	top of heap == 3 at index 1, max still in window,
	add nums[3] == -3 to heap
	heap: [(3,1), (1,0) (-1,2)]
          (3,1)
        /       \
      (1,0)	   (-1,2)
     /
   (-3,3)
	max: [3,3]

	i:4, window [2..4]
	top of heap == 3 at index 1, max out of window,
	remove top of heap,
	heap: [(3,1), (1,0) (-1,2)]
          (3,1)                           (1,0)
        /       \          -->          /      \     
      (1,0)	   (-1,2)                  (-3.3)  (-1,2) 
     /
   (-3,3)
	add nums[4] == 5 to heap
          (1,0)                           (5,4)
        /       \                        /     \
      (-3,3)	   (-1,2)      -->     (1,0)   (-1,2)
     /                                 / 
   (5,4)                             (-3,3)            
	max: [3,3,5]


	i:5, window [3..5]
	top of heap == 5 at index 4, still in window,
	add nums[5] to heap
          (5,4)
        /       \
      (3,5)	   (-1,2)
     /    \
   (-3,3) (1,0)
	max: [3,3,5,5]

	i:6, window [4..6]
	top of heap == 5 at index 4, still in window,
	add nums[6] to heap
          (6,6)
        /       \
      (3,5)	   (5,4)
     /    \      /
   (-3,3) (1,0) (-1,2)
	max: [3,3,5,5,6]

	i:7, window [5..7]
	top of heap == 6 at index 6, still in window,
	add nums[7] to heap
          (6,6)
        /       \
      (3,5)	   (5,4)
     /    \      /    \
   (-3,3) (1,0) (-1,2) (7,7) 

          |
		  v

          (7,7)
        /       \
      (3,5)	   (6,6)
     /    \      /    \
   (-3,3) (1,0) (-1,2) (5,4) 

	max: [3,3,5,5,6,7]
'''

from data_structures.heap.maxheap import MaxHeap
def maxSlidingWindow(nums, k):
	if not nums:
		return []

	h = MaxHeap()

	for i in xrange(k):
		h.add((nums[i], i))
		
	max_list = [h.peek()[0]]

	for i in xrange(1,len(nums)-k+1):
		# max is to the left of window
		# remove all previous max items that are no longer in the window,
		# from the heap
		while h and h.peek()[1] < i:
			h.remove()

		# add item at end of current window, i+k-1, to the heap
		h.add((nums[i+k-1], i+k-1))

		# add current window's max from the top of the heap
		max_list.append(h.peek()[0])

	return max_list


if __name__ == '__main__':
	assert maxSlidingWindow([5,1,4,3], 2) == [5,4,4]
	assert maxSlidingWindow([4,3,2,1], 2) == [4,3,2]
	assert maxSlidingWindow([4,3,2,1], 3) == [4,3]
	assert maxSlidingWindow([1,3,-1,-3,5,3,6,7], 3) == [3,3,5,5,6,7]
	assert maxSlidingWindow([9,10,9,-7,-4,-8,2,-6], 5) == [10, 10, 9, 2]
	assert maxSlidingWindow([1, -1], 1) == [1, -1]
	assert maxSlidingWindow([4,3,2,1,2,3,4], 3) == [4,3,2,3,4]
	
