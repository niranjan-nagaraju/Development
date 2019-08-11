from heap import Heap

class MaxHeap(Heap):
	def __init__(self, comparatorfn=None):
		self.items = []
		_comparatorfn = comparatorfn if comparatorfn else cmp
		self.comparatorfn = lambda a, b: _comparatorfn(b, a)
		self.isHeap = self.isHeap_r # use recursive version by default
		self.isMaxHeap = self.isHeap_r


	'''
	decreaseKey() is invalid for a max heap
	'''
	def decreaseKey(self, i, new):
		raise AttributeError('Heap object has no attribute decreaseKey')

	
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



if __name__ == '__main__':
	caught_exception = False
	try:
		h = MaxHeap()
		assert h.decreaseKey(0, 5)
	except AttributeError:
		caught_exception = True

	assert caught_exception == True

