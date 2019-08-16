'''
https://leetcode.com/problems/find-k-pairs-with-smallest-sums/

You are given two integer arrays nums1 and nums2 sorted in ascending order and an integer k.

Define a pair (u,v) which consists of one element from the first array and one element from the second array.

Find the k pairs (u1,v1),(u2,v2) ...(uk,vk) with the smallest sums.

Example 1:
Input: nums1 = [1,7,11], nums2 = [2,4,6], k = 3
Output: [[1,2],[1,4],[1,6]] 
Explanation: The first 3 pairs are returned from the sequence: 
             [1,2],[1,4],[1,6],[7,2],[7,4],[11,2],[7,6],[11,4],[11,6]

Example 2:
Input: nums1 = [1,1,2], nums2 = [1,2,3], k = 2
Output: [1,1],[1,1]
Explanation: The first 2 pairs are returned from the sequence: 
             [1,1],[1,1],[1,2],[2,1],[1,2],[2,2],[1,3],[1,3],[2,3]

Example 3:
Input: nums1 = [1,2], nums2 = [3], k = 3
Output: [1,3],[2,3]
Explanation: All possible pairs are returned from the sequence: [1,3],[2,3]
'''


# Priority Queue using a Heap
class PriorityQueueHeap():
	def __init__(self):
		self.parent = lambda i: (i-1)/2
		self.left = lambda i: 2*i+1
		self.right = lambda i: 2*i+2

		self.items = []

	def bubble_up(self, i):
		while (i > 0) and self.items[i] < self.items[self.parent(i)]:
			self.items[i], self.items[self.parent(i)] = self.items[self.parent(i)], self.items[i]
			i = self.parent(i)


	def bubble_down(self, n, i=0):
		l, r = self.left(i), self.right(i)
		smaller_of = lambda i,j: i if self.items[i] < self.items[j] else j

		smallest = i
		# Find the smallest of left, right and root items
		if l < n:
			smallest = smaller_of(smallest, l)

		if r < n:
			smallest = smaller_of(smallest, r)

		# swap root of the subtree with the smallest of the left and right children
		if (smallest != i):
			self.items[i], self.items[smallest] = self.items[smallest], self.items[i]
			self.bubble_down(n, smallest)



	def push(self, item):
		self.items.append(item)
		self.bubble_up(len(self.items)-1)


	def pop(self):
		top = self.items[0]
		self.items[0] = self.items[-1]
		self.items.pop()
		self.bubble_down(len(self.items), 0)
		return top


class Solution(object):
	def kSmallestPairs(self, nums1, nums2, k):
		"""
		:type nums1: List[int]
		:type nums2: List[int]
		:type k: int
		:rtype: List[List[int]]
		"""

		# maximum numbers we need to look into
		# either of nums1 and nums2 is k each
		# that gives us k^2 possible sums to find k-smallest sums
		# Push them all into a heap and extract min k-times
		# to get the k-smallest sums
		try:
			pq = PriorityQueueHeap()
			pairs = []
			for i in xrange(min(k, len(nums1))):
				for j in xrange(min(k, len(nums2))):
					pq.push((nums1[i] + nums2[j], (nums1[i], nums2[j])))

			for i in xrange(k):
					_, (a,b) = pq.pop()
					pairs.append([a,b])

		except IndexError:
			pass

		return pairs


if __name__ == '__main__':
	s = Solution()
	assert s.kSmallestPairs([1,7,11],  [2,4,6], 3) == [[1,2],[1,4],[1,6]]
	assert s.kSmallestPairs([1,1,2], [1,2,3], 2) == [[1,1],[1,1]]
	assert s.kSmallestPairs([1,2], [3], 3) == [[1,3],[2,3]]

