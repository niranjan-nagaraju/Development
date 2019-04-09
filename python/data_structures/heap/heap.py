'''
A heap data structure implemented using a list
'''


class Heap(object):
	def __init__(self, comparatorfn=None):
		self.items = []
		self.comparatorfn = comparatorfn if comparatorfn else cmp


	def __str__(self):
		return str(self.items)

	def __repr__(self):
		return repr(self.items)


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
	def bubble_up(self):
		i = len(self.items)-1
		while (i > 0) and self.items[i] < self.items[self.parent(i)]:
			self.items[i], self.items[self.parent(i)] = self.items[self.parent(i)], self.items[i]
			i = self.parent(i)


	'''
	Add an item to the heap
	Start by adding item to the end to the list,
	then bubble up until it's in its rightful place, and 
	the heap property is restored.
	'''
	def add(self, item):
		self.items.append(item)
		self.bubble_up()

