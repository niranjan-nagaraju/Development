from heap import Heap

class MaxHeap(Heap):
	def __init__(self, comparatorfn=None):
		Heap.__init__(self)

		# flip comparator function to flip > and <
		# so a minheap becomes a maxheap
		_comparatorfn = comparatorfn if comparatorfn else cmp
		self.comparatorfn = lambda a, b: _comparatorfn(b, a)
		self.isMaxHeap = self.isHeap_r


	'''
	decreaseKey() on a maxHeap is the same as increaseKey() on a minHeap
	The current item gets bubbled down
	'''
	def decreaseKey(self, i, new):
		try:
			Heap.increaseKey(self, i, new)
		except ValueError:
			raise ValueError("%s: %s() - New key should be lesser than current value" %('ValueError', "decreaseKey"))

	
	'''
	Inrease key of item at index 'i' to new
	new should be > items[i]
	'''
	def increaseKey(self, i, new):
		try:
			Heap.decreaseKey(self, i, new)
		except ValueError:
			raise ValueError("%s: %s() - New key should be greater than current value" %('ValueError', "increaseKey"))



	'''
	Use heap to sort an input list
	'''
	@classmethod
	def sort(cls, inList):
		heap = cls.build_heap(inList)
		for i in xrange(len(inList)-1, -1, -1):
			inList[0], inList[i] = inList[i], inList[0]
			heap.bubble_down(i, 0)

		return inList



