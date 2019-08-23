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


class MaxHeap(object):
	def __init__(self):
		self.items = []
		self.parent = lambda i: (i-1)/2
		self.left = lambda i: 2*i+1
		self.right = lambda i: 2*i+2

	'''
	Bubble up item at index, 'i' all the way up
	till the heap property is restored
	'''
	def bubble_up(self, i):
		while (i > 0) and cmp(self.items[i], self.items[self.parent(i)]) > 0:
			self.items[i], self.items[self.parent(i)] = self.items[self.parent(i)], self.items[i]
			i = self.parent(i)


	'''
	Bubble-down an item at 'i'
	to its rightful place, and the heap property is restored
	Assumes left and right subtrees are already heaps.

	Typically used after extracting the top of the heap,
	the last item in the heap replaces the top of the heap
	and is bubbled down, until heap property is restored.
	'''
	def bubble_down(self, n, i=0):
		l, r = self.left(i), self.right(i)
		larger_of = lambda i,j: i if cmp(self.items[i], self.items[j])>0 else j

		largest = i
		# Find the largest of left, right and root items
		if l < n:
			largest = larger_of(largest, l)

		if r < n:
			largest = larger_of(largest, r)

		# swap root of the subtree with the largest of the left and right children
		if (largest != i):
			self.items[i], self.items[largest] = self.items[largest], self.items[i]
			self.bubble_down(n, largest)



	'''
	Add an item to the heap
	Start by adding item to the end to the list,
	then bubble up until it's in its rightful place, and 
	the heap property is restored.
	'''
	def add(self, item):
		self.items.append(item)
		self.bubble_up(len(self.items)-1)

	'''
	remove the top of the heap
	'''
	def remove(self):
		top = self.items[0]
		self.items[0] = self.items[-1]
		self.items.pop()
		self.bubble_down(len(self.items), 0)

		return top


class Solution(object):
	def maxSlidingWindow(self, nums, k):
		"""
		:type nums: List[int]
		:type k: int
		:rtype: List[int]
		"""

		if not nums:
			return []

		h = MaxHeap()

		for i in xrange(k):
			h.add((nums[i], i))
			
		max_list = [h.items[0][0]]

		for i in xrange(1,len(nums)-k+1):
			# max is to the left of window
			# remove all previous max items that are no longer in the window,
			# from the heap
			while h.items and h.items[0][1] < i:
				h.remove()

			# add item at end of current window, i+k-1, to the heap
			h.add((nums[i+k-1], i+k-1))

			# add current window's max from the top of the heap
			max_list.append(h.items[0][0])

		return max_list


if __name__ == '__main__':
	s = Solution()
	assert s.maxSlidingWindow([5,1,4,3], 2) == [5,4,4]
	assert s.maxSlidingWindow([4,3,2,1], 2) == [4,3,2]
	assert s.maxSlidingWindow([4,3,2,1], 3) == [4,3]
	assert s.maxSlidingWindow([1,3,-1,-3,5,3,6,7], 3) == [3,3,5,5,6,7]
	assert s.maxSlidingWindow([9,10,9,-7,-4,-8,2,-6], 5) == [10, 10, 9, 2]
	assert s.maxSlidingWindow([1, -1], 1) == [1, -1]
	
