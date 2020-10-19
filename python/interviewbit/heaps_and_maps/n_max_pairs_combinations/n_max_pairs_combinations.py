'''
https://www.interviewbit.com/problems/n-max-pair-combinations/

N max pair combinations

Given two arrays A & B of size N each.
Find the maximum N elements from the sum combinations (Ai + Bj) formed from elements in array A and B.

For example if A = [1,2], B = [3,4], then possible pair sums can be 1+3 = 4 , 1+4=5 , 2+3=5 , 2+4=6
and maximum 2 elements are 6, 5

Example:

	N = 4
	a[]={1,4,2,3}
	b[]={2,5,1,6}

Maximum 4 elements of combinations sum are
	10   (4+6), 
	9    (3+6),
	9    (4+5),
	8    (2+6)
'''


'''
Solution: (Brute-force solution)
	1. Generate N*N pair-sums.
	2. Store them in a max-heap
	3. Extract max-heap 'N' times, and return the results.
'''
class MaxHeap:
	def __init__(self, capacity):
		self.capacity = capacity
		self.items = [None]*(capacity)
		self.size = 0
		self.parent = lambda i: (i-1)/2
		self.left = lambda i: (2*i+1)
		self.right = lambda i: (2*i+2)


	def __len__(self):
		return self.size


	# Add an item to the heap
	def add(self, item):
		self.items[self.size] = item
		self.bubble_up()
		self.size += 1


	# Extract the current max from the top of the heap
	def extractMax(self):
		item = self.items[0]
		self.items[0] = self.items[self.size-1]
		self.size -= 1
		self.bubble_down()
		return item


	# Bubble up item at the end of the heap
	# till the max-heap property is restored
	def bubble_up(self):
		n = self.size
		while n and self.items[self.parent(n)] <= self.items[n]:
			self.items[self.parent(n)], self.items[n] = \
					self.items[n], self.items[self.parent(n)]
			n = self.parent(n)


	# Bubble down item at the top of the heap
	# till the max-heap property is restored
	def bubble_down(self, i=0):
		if i >= self.size:
			return

		l = self.left(i)
		r = self.right(i)

		if l >= self.size:
			# i is a leaf-node
			return

		maximum = i
		if self.items[i] < self.items[l]:
			maximum = l

		if r < self.size:
			if self.items[maximum] < self.items[r]:
				maximum = r

		# swap i with its maximum of its children
		# if they were greater
		if maximum == i:
			return

		self.items[maximum], self.items[i] = self.items[i], self.items[maximum]
		self.bubble_down(maximum)




class Solution:
	def findMaxPairSums(self, A, B):
		if not A:
			return []

		heap = MaxHeap(len(A)*len(A))

		for x in A:
			for y in B:
				heap.add(x+y)

		return [heap.extractMax() for _ in xrange(len(A))]



if __name__ == '__main__':
	h = MaxHeap(10)
	h.add(1)
	h.add(3)
	h.add(2)
	h.add(6)
	h.add(4)
	h.add(5)
	h.add(7)
	assert h.items[:h.size] == [7, 4, 6, 1, 3, 2, 5]

	i = 7
	while h:
		assert h.extractMax() == i
		i -= 1


	s = Solution()
	assert s.findMaxPairSums([1,2], [3,4]) == [6,5]
	assert s.findMaxPairSums([1,4,2,3], [2,5,1,6]) == [10,9,9,8]

