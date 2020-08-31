'''
Indexed Min heap:
	Min heap that supports O(1) find(key)
	Override hash() function of the object being added to the heap to find the object based on a desired key.
'''
from heap import Heap, HeapEmptyError
class IndexedHeap(Heap):
	def __init__(self, comparatorfn=cmp):
		Heap.__init__(self, comparatorfn)
		self.lookup = {}


	def __str__(self):
		return str(Heap.__str__(self)) + '\n' + str(self.lookup)

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
	Bubble up item at index, 'i' all the way up
	till the heap property is restored

	used by promote()/decreaseKey, and add()
	'''
	def bubble_up(self, i):
		while (i > 0) and self.comparatorfn(self.items[i], self.items[self.parent(i)]) < 0:
			self.items[i], self.items[self.parent(i)] = self.items[self.parent(i)], self.items[i]
			self.lookup[hash(self.items[self.parent(i)])] = self.parent(i)
			self.lookup[hash(self.items[i])] = i
			i = self.parent(i)


	'''
	Bubble-down an item at 'i'
	to its rightful place, and the heap property is restored
	Assumes left and right subtrees are already heaps.
	
	used by remove(), demote()/increaseKey()
	'''
	def bubble_down(self, n, i=0):
		l, r = self.left(i), self.right(i)

		# 'i' doesn't have a left-child
		#  bubble-down complete
		if not l < n:
			return

		smaller_of = lambda i,j: i if self.comparatorfn(self.items[i], self.items[j])<0 else j
		if not r < n:
			# No right-child, l is the smaller child
			idx = l
		else:
			# Pick smaller of the left and right children to swap with parent, i
			idx = smaller_of(l, r)

		if (smaller_of(i, idx) != i):
			# item at 'i' > its children => bubble-down
			self.items[i], self.items[idx] = self.items[idx], self.items[i]
			self.lookup[hash(self.items[idx])] = idx
			self.lookup[hash(self.items[i])] = i
			self.bubble_down(n, idx)


	'''
	Add an item to the heap
	Start by adding item to the end to the list,
	then bubble up until it's in its rightful place, and 
	the heap property is restored.
	'''
	def add(self, item):
		self.items.append(item)
		self.lookup[hash(item)] = len(self.items)-1
		self.bubble_up(len(self.items)-1)



	'''
	Return the index where 'item' is in the heap-array
	'''
	def _find_idx(self, item):
		return self.lookup.get(hash(item))


	'''
	Return the item in the priority queue based on the matching key
	'''
	def find(self, key):
		idx = self._find_idx(key)
		if idx is None:
			return None
		return self.items[idx]


	'''
	remove the top of the heap
	'''
	@check_empty
	def remove(self):
		top = self.items[0]
		self.items[0] = self.items[-1]
		self.items.pop()

		if self.items:
			self.lookup[hash(self.items[0])] = 0
			self.bubble_down(len(self.items), 0)

		# Remove the entry from the lookup table as well
		del self.lookup[hash(top)]
		return top

	'''
	Decrease key of item at index 'i' to new
	new should be <  items[i]
	'''
	def decreaseKey(self, i, new):
		if not self.comparatorfn(new, self.items[i]) < 0:
			raise ValueError("%s: %s() - New key should be less than current value" %('ValueError', self.decreaseKey.__name__))

		# remove previous key from the lookup table on decreaseKey
		# the 'new' key should replace it
		del self.lookup[hash(self.items[i])]
		self.items[i] = new
		self.lookup[hash(new)] = i
		self.bubble_up(i)


	'''
	Increase key of item at index 'i' to new
	new should be >  items[i]
	NOTE: decreaseKey() is the common operation because it promotes an item up the tree in a heap
	However, there's no reason why increaseKey() cannot be supported.
	NOTE: increaseKey() promotes up the tree for a maxheap.
	'''
	def increaseKey(self, i, new):
		if not self.comparatorfn(new, self.items[i]) > 0:
			raise ValueError("%s: %s() - New key should be greater than current value" %('ValueError', self.increaseKey.__name__))

		# remove previous key from the lookup table on increaseKey
		# the 'new' key should replace it
		del self.lookup[hash(self.items[i])]
		self.items[i] = new
		self.lookup[hash(new)] = i
		self.bubble_down(len(self.items), i)



if __name__ == '__main__':
	ih = IndexedHeap()

	ih.add(3)
	assert len(ih) == 1
	assert ih.items == [3]
	assert ih.lookup == {3: 0}
	assert ih._find_idx(3) == 0

	ih.add(2)
	assert len(ih) == 2
	assert ih.items == [2, 3]
	assert ih.lookup == {2:0, 3: 1}
	assert ih._find_idx(3) == 1
	assert ih._find_idx(2) == 0

	ih.add(1)
	assert len(ih) == 3
	assert ih.items == [1, 3, 2]
	assert ih.lookup == {1:0, 2:2, 3:1}
	assert ih._find_idx(3) == 1
	assert ih.find(3) == 3 # returns the entry in the heap matching the key

	ih.add(0)
	assert len(ih) == 4
	assert ih.items == [0, 1, 2, 3]
	assert ih.lookup == {0:0, 1:1,  2:2, 3:3}

	assert ih.remove() == 0
	assert len(ih) == 3
	assert ih.items == [1, 3, 2]
	assert ih.lookup == {1:0,  2:2, 3:1}

	ih.decreaseKey(2, 0)
	assert len(ih) == 3
	assert ih.items == [0, 3, 1]
	assert ih.lookup == {0:0, 1:2, 3:1}
	assert ih._find_idx(2) == None
	assert ih._find_idx(0) == 0

	ih.increaseKey(2, 4)  # change key at index '2' -> 1 to 4
	assert len(ih) == 3
	assert ih.items == [0, 3, 4]
	assert ih.lookup == {0:0, 3:1, 4:2}
	assert ih._find_idx(1) == None
	assert ih.find(1) == None
	assert ih._find_idx(4) == 2

	ih.increaseKey(0, 5)  # change key at index '0' -> 0 to 5
	assert len(ih) == 3
	assert ih.items == [3, 5, 4]
	assert ih.lookup == {3:0, 4:2, 5:1}

	assert ih.remove() == 3
	assert len(ih) == 2
	assert ih.items == [4,5]
	assert ih.lookup == {4:0, 5:1}

	assert ih.remove() == 4
	assert len(ih) == 1
	assert ih.items == [5]
	assert ih.lookup == {5:0}

	assert ih.remove() == 5
	assert len(ih) == 0
	assert ih.items == []
	assert ih.lookup == {}

