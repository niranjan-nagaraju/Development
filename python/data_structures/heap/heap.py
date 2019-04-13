'''
A heap data structure implemented using a list
'''

'''
Heap Empty exception
'''
class HeapEmptyError(Exception):
	def __init__(self, message='Heap is empty!'):
		self.message = message

	def __str__(self):
		return self.message


class Heap(object):
	def __init__(self, comparatorfn=None):
		self.items = []
		self.comparatorfn = comparatorfn if comparatorfn else cmp


	# A default print function if no aggregator is provided
	# for traversal functions
	@staticmethod
	def _default_printfn(item):
		print str(item) + " ",

	def __str__(self):
		return str(self.items)

	def __repr__(self):
		return repr(self.items)


	def __len__(self):
		return len(self.items)

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


	@check_empty
	def peek(self):
		return self.items[0]


	@staticmethod
	def parent(i):
		return (i-1)/2

	@staticmethod
	def left(i):
		return 2*i+1

	@staticmethod
	def right(i):
		return 2*i+2


	'''
	Bubble up item at the end of the heap
	till the heap property is restored
	'''
	def bubble_up(self, n):
		i = n-1
		while (i > 0) and self.items[i] < self.items[self.parent(i)]:
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
	def bubble_down(self, i):
		try:
			l, r = self.left(i), self.right(i)
			smaller_of = lambda i,j: i if self.comparatorfn(self.items[i], self.items[j])<0 else j

			# Find the smallest of left, right and root items
			smallest = smaller_of(l, r)
			smallest = smaller_of(smallest, i)

			# swap root of the subtree with the smallest of the left and right children
			if (smallest != i):
				self.items[i], self.items[smallest] = self.items[smallest], self.items[i]
				self.bubble_down(smallest)
		except IndexError:
			# overshot left and right subtree indices beyond the list bounds
			return



	'''
	Add an item to the heap
	Start by adding item to the end to the list,
	then bubble up until it's in its rightful place, and 
	the heap property is restored.
	'''
	def add(self, item):
		self.items.append(item)
		self.bubble_up(len(self.items))


	
	'''
	remove the top of the heap
	'''
	@check_empty
	def remove(self):
		top = self.items[0]
		self.items[0] = self.items[-1]
		self.items.pop()
		self.bubble_down(0)

		return top



	'''
	Decrease key of item at index 'i' to new
	new should be <  items[i]
	'''
	def decreaseKey(self, i, new):
		pass

	'''
	Uses bubble_down to build heap out of a list/iterable starting from 
	first non-leaf node from the bottom 
	'''
	def build_heap(l):
		# heap[(n-1)/2 .. 0] are non-leaf nodes 
		# e.g.
		#                  0
		#               /     \
		#              1       2 
		#             / \     / \
		#            3  4     5  6
		#            
		# foreach i: (n-1)/2 .. 0, bubble_down(i)
		pass


	'''
	Repeatedly call remove() to get a sorted list
	'''
	def sorted(self, aggregatorfn=None):
		if not aggregatorfn:
			aggregatorfn = _default_printfn

		while self:
			aggregatorfn(self.remove())


