'''
Indexed Priority Queue
	Min-priority queue with O(1) find(key)
'''

from data_structures.heap.indexed_heap import IndexedHeap

# Indexed Priority Queue using an Indexed Min-Heap
class IndexedPriorityQueue(IndexedHeap):
	def __init__(self, comparatorfn=None):
		IndexedHeap.__init__(self, comparatorfn)


	# Remove the top of the heap, and its entry from the lookup table
	def dequeue(self):
		item = self.remove()
		del self.lookup[item]
		return item


	# Add an item to the priority queue with an associated priority level
	def enqueue(self, item):
		self.add(item)


	# Increase priority of item at index 'i' based on 'new' value
	# new should be < current
	def promote(self, item, new):
		i = self.find(item)
		if i is None:
			raise KeyError("Couldnt locate {}".format(str(item)))
		self.decreaseKey(i, new)
		# remove previous key from the lookup table on promote
		# the 'new' key should replace it
		del self.lookup[item]


	# Decrease priority of item at index 'i' based on 'new' value
	# new should be > current
	def demote(self, item, new):
		i = self.find(item)
		if i is None:
			raise KeyError("Couldnt locate {}".format(str(item)))
		self.increaseKey(i, new)
		# remove previous key from the lookup table on demote
		# the 'new' key should replace it
		del self.lookup[item]





if __name__ == '__main__':
	# A sample node item that could
	# be used in a Dijsktra's algorithm
	class PQItem(object):
		def __init__(self, vertex, distance, from_):
			self.vertex = vertex
			self.distance = distance
			self.from_ = from_


		def __str__(self):
			return str((self.vertex, self.distance, self.from_))

		def __repr__(self):
			return self.__str__()


		# Two PQItems are compared based on their 'distance' fields
		# for ordering
		# This determines their position in the heap/priority-queue
		# NOTE: __eq__ will be called first to check for equality before __cmp__
		def __cmp__(self, other):
			return cmp(self.distance, other.distance)


		# Two PQItems are identical if they share the same internal state
		def __eq__(self, other):
			return (self.vertex, self.distance, self.from_) == (other.vertex, other.distance, other.from_)

		# Overriding __eq__ breaks hash(), so implement a hash function that returns the same hash
		# for two pqitems that share the same internal state
		def __hash__(self):
			return hash((self.vertex, self.distance, self.from_))

	
	ipq = IndexedPriorityQueue()
	ipq.enqueue(PQItem(1, 24, 2))
	ipq.enqueue(PQItem(2, 12, 1))
	ipq.enqueue(PQItem(3, 6, 5))

	assert ipq.items == [PQItem(3, 6, 5), PQItem(1, 24, 2), PQItem(2, 12, 1),]
	assert map(ipq.find, [PQItem(3, 6, 5), PQItem(1, 24, 2), PQItem(2, 12, 1),]) == [0,1,2]

	assert ipq.peek() == PQItem(3, 6, 5)
	assert len(ipq) == 3

	ipq.enqueue(PQItem(4, 12, 6))
	assert len(ipq) == 4
	assert ipq.items == [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(2, 12, 1), PQItem(1, 24, 2)]
	assert map(ipq.find, [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(1, 24, 2), PQItem(2, 12, 1),]) == [0,1,3,2]

	assert ipq.find(PQItem(1, 24, 2)) == 3
	ipq.promote(PQItem(1, 24, 2), PQItem(1, 7, 2))
	assert ipq.items == [PQItem(3, 6, 5), PQItem(1, 7, 2), PQItem(2, 12, 1), PQItem(4, 12, 6), ]
	assert map(ipq.find, [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(1, 24, 2), PQItem(2, 12, 1), PQItem(1, 7, 2)]) == [0, 3, None, 2, 1]

	assert ipq.dequeue() == PQItem(3,6,5)
	assert ipq.items == [PQItem(1, 7, 2), PQItem(4, 12, 6), PQItem(2, 12, 1), ]

	assert map(ipq.find, [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(1, 24, 2), PQItem(2, 12, 1), PQItem(1, 7, 2)]) == [None, 1, None, 2, 0]

	assert ipq.peek() == PQItem(1, 7, 2)
	assert ipq.dequeue() == PQItem(1, 7, 2)

	ipq.promote(PQItem(2,12,1), PQItem(2,3,1))
	assert ipq.peek() == PQItem(2, 3, 1)
	assert ipq.dequeue() == PQItem(2, 3, 1)
	assert ipq.peek() == PQItem(4, 12, 6)
	assert ipq.dequeue() == PQItem(4, 12, 6)

	assert len(ipq) == 0

