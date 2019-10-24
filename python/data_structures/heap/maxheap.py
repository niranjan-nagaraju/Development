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
	decorator to ensure heap is not empty
	on heap operations that modify the heap
	'''
	def check_empty(func):
		def f(self, *args, **kwargs):
			if len(self.items) == 0:
				raise HeapEmptyError("HeapEmptyError: '%s(): Heap is empty'" %(func.__name__))
			rv = func(self, *args, **kwargs)
			return rv
		return f



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
	[] : update item at index i
	override parent class' minheap based update
	'''
	@check_empty
	def __setitem__(self, i, value):
		if (i < 0 and i >= len(self.items)):
			raise IndexError

		if self.comparatorfn(value, self.items[i]) < 0:
			# If new value is greater, Use increaseKey() to bubble down
			self.increaseKey(i, value)
		elif self.comparatorfn(value, self.items[i]) > 0:
			# If new value is lesser, Use decreaseKey() to bubble up
			self.decreaseKey(i, value)
		else:
			self.items[i] = value



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



