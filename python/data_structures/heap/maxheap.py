from heap import Heap

class MaxHeap(Heap):
	def __init__(self, comparatorfn=None):
		self.items = []
		_comparatorfn = comparatorfn if comparatorfn else cmp
		self.comparatorfn = lambda a, b: _comparatorfn(b, a)
		self.isHeap = self.isHeap_r # use recursive version by default
		self.isMaxHeap = self.isHeap_r

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




if __name__ == '__main__':
	caught_exception = False
	try:
		h = MaxHeap()
		assert h.decreaseKey(0, 5)
	except AttributeError:
		caught_exception = True

	assert caught_exception == True

