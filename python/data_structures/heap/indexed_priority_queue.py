'''
Indexed Priority Queue
	Min-priority queue with O(1) find(key)
	Override hash() function of the object being added to the heap to find the object based on a desired key.
'''

from data_structures.heap.indexed_heap import IndexedHeap

# Indexed Priority Queue using an Indexed Min-Heap
class IndexedPriorityQueue(IndexedHeap):
	def __init__(self, comparatorfn=None):
		IndexedHeap.__init__(self, comparatorfn)


	'''
	Remove the top of the heap, and its entry from the lookup table
	'''
	def dequeue(self):
		return self.remove()


	'''
	Add an item to the priority queue with an associated priority level
	'''
	def enqueue(self, item):
		self.add(item)

	
	'''
	Increase priority of item at index 'i' based on 'new' value
	new should be < current
	'''
	def promote(self, item, new):
		i = self._find_idx(item)
		if i is None:
			raise KeyError("Couldnt locate {}".format(str(item)))
		self.decreaseKey(i, new)


	'''
	Decrease priority of item at index 'i' based on 'new' value
	new should be > current
	'''
	def demote(self, item, new):
		i = self._find_idx(item)
		if i is None:
			raise KeyError("Couldnt locate {}".format(str(item)))
		self.increaseKey(i, new)



if __name__ == '__main__':
	# A sample node item that could
	# be used in a Dijkstra's algorithm
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

		# Override hash(PQItem) so we can lookup by vertex
		# the indexed priority queue will store a mapping
		# of vertex -> node index in the heap
		def __hash__(self):
			return hash(self.vertex)

	
	ipq = IndexedPriorityQueue()
	ipq.enqueue(PQItem(1, 24, 2))
	ipq.enqueue(PQItem(2, 12, 1))
	ipq.enqueue(PQItem(3, 6, 5))

	assert ipq.items == [PQItem(3, 6, 5), PQItem(1, 24, 2), PQItem(2, 12, 1),]
	assert map(ipq._find_idx, [PQItem(3, 6, 5), PQItem(1, 24, 2), PQItem(2, 12, 1),]) == [0,1,2]
	assert ipq._find_idx(PQItem(3, "foo", "bar")) == 0 # Lookup table is key-ed using pqitem's vertex
	assert ipq._find_idx(3) == 0 # Lookup table is key-ed using pqitem's vertex, can lookup using only vertex
	assert ipq.find(3) == PQItem(3,6,5) # Lookup table is key-ed using pqitem's vertex, can lookup using only vertex

	assert ipq.peek() == PQItem(3, 6, 5)
	assert len(ipq) == 3

	ipq.enqueue(PQItem(4, 12, 6))
	assert len(ipq) == 4
	assert ipq.items == [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(2, 12, 1), PQItem(1, 24, 2)]
	assert map(ipq._find_idx, [PQItem(3, 6, 5), PQItem(4, 12, 6), PQItem(1, 24, 2), PQItem(2, 12, 1),]) == [0,1,3,2]

	assert ipq._find_idx(PQItem(1, 24, 2)) == 3
	ipq.promote(PQItem(1, 24, 2), PQItem(1, 7, 3))
	assert ipq.find(1) == PQItem(1,7,3)
	assert ipq.items == [PQItem(3, 6, 5), PQItem(1, 7, 3), PQItem(2, 12, 1), PQItem(4, 12, 6), ]
	assert map(ipq._find_idx, [3, 4, 1, 2]) == [0, 3, 1, 2]

	assert ipq.dequeue() == PQItem(3,6,5)
	assert ipq.items == [PQItem(1, 7, 3), PQItem(4, 12, 6), PQItem(2, 12, 1), ]
	assert map(ipq._find_idx, [3, 4, 1, 2]) == [None, 1, 0, 2]

	assert ipq.peek() == PQItem(1, 7, 3)
	assert ipq.dequeue() == PQItem(1, 7, 3)
	assert ipq.find(1) == None # just dequeued
	assert ipq.find(PQItem(1,7,3)) == None

	ipq.promote(PQItem(2,12,1), PQItem(2,3,1))
	assert ipq.find(2) == PQItem(2,3,1)
	assert ipq.items == [PQItem(2,3,1), PQItem(4,12,6)]

	ipq.demote(PQItem(2,3,1), PQItem(2,13,1))
	assert ipq.find(2) == PQItem(2,13,1)
	assert ipq.items == [PQItem(4,12,6), PQItem(2,13,1)]

	assert ipq.peek() == PQItem(4, 12, 6)
	assert ipq.dequeue() == PQItem(4, 12, 6)
	assert ipq.peek() == PQItem(2, 13, 1)
	assert ipq.dequeue() == PQItem(2, 13, 1)
	assert ipq.find(2) == None
	assert ipq.find(PQItem(2,13,1)) == None

	assert len(ipq) == 0
	assert ipq.items == []
	assert ipq.lookup == {}

