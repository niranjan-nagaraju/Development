'''
An Indexed-Queue based off of a regular Indexed-SLL

IndexedQueue:
	- enqueue(key, value): push an item(key, value) to the back of the SLL-Queue. {O(1) time}
	- dequeue(): Remove item(key, value) from the front of the SLL-Queue and return it. {O(1) time}
	- reEnqueue(key, value): Remove(key) from anywhere in the Queue, and add it back at the end.
	- get(key): return value corresponding to matching key in the SLL-Queue. {O(1) time}

	- remove(key): *NOT AVAILABLE*
	             remove item(key,value) matching key from the SLL-Queue, {O(1) time}*
					except when key is part of the tail-node (which uses O(n) time).

			 PS: It is assumed that remove(tail) is *not called directly* but only as part of reEnqueue() that does remove+enqueue.
				 Use in implementations like LRU-cache where remove(tail) is redundant since reEnqueue(tail) is a NO-OP.

			 PPS: If your usage/Indexed-Queue implementation needs the tail node to be removed,
				 Please see, Indexed Queue using an SLL with tail-sentinel.
'''

from indexed_sll_regular import IndexedSLL

class IndexedQueue(IndexedSLL):
	def __init__(self):
		IndexedSLL.__init__(self)
		self.enqueue = self.push_back
		self.dequeue = self.pop_front


	# Make remove() inherited from IndexedSLL, unusable.
	def remove(self, key):
		raise AttributeError("`{}` has no attribute named `{}`"\
				.format(self.__class__.__name__, "remove"))


	# Re-enqueue 'key' to the back of the queue
	# Assumes the key is already in the queue
	# Caller is expected to make the check and call
	# enqueue() vs reEnqueue() accordingly
	def reEnqueue(self, key, value=None):
		node = self.lookup.get(key)
		item = node.item
		if value is not None:
			# If a new value is provided
			# for an existing key,
			# update its value in the queue
			item = (key, value)

		if node is self.tail:
			# re-enqueuing queue's last node
			# is redundant
			# However, if its key's value has changed
			# update it in-place and return
			node.item = item
			return

		IndexedSLL.remove(self, key)
		self.enqueue(key, item[1])



if __name__ == '__main__':
	iq = IndexedQueue()
	iq.enqueue(1, 'one')

	try:
		error_raised = False
		iq.remove(1234)
	except AttributeError:
		error_raised = True
	assert error_raised == True

	assert len(iq) == 1
	assert iq.get(1) == 'one'
	iq.reEnqueue(1, 100)
	assert len(iq) == 1
	assert iq.get(1) == 100

	iq.enqueue(2, 200)
	iq.enqueue(4, 400)
	assert len(iq) == 3
	assert iq.get(4) == 400

	assert iq.dequeue() == (1, 100)
	assert len(iq) == 2
	assert iq.get(1) == None
	assert iq.get('blah') == None

	iq.enqueue(1, 600) # 2 -> 4 -> 1
	assert len(iq) == 3
	assert iq.get(1) == 600
	assert str(iq).split('\n')[0] == "['(2, 200)', '(4, 400)', '(1, 600)']"

	iq.reEnqueue(4)
	assert len(iq) == 3
	assert str(iq).split('\n')[0] == "['(2, 200)', '(1, 600)', '(4, 400)']"

	iq.reEnqueue(2)
	assert len(iq) == 3
	assert str(iq).split('\n')[0] == "['(1, 600)', '(4, 400)', '(2, 200)']"

	assert iq.tail.item == (2, 200)
	iq.reEnqueue(2)
	assert len(iq) == 3
	assert str(iq).split('\n')[0] == "['(1, 600)', '(4, 400)', '(2, 200)']"

	iq.reEnqueue(1, 111)
	assert len(iq) == 3
	assert str(iq).split('\n')[0] == "['(4, 400)', '(2, 200)', '(1, 111)']"

	assert iq.dequeue() == (4,400)
	assert len(iq) == 2
	assert iq.dequeue() == (2,200)
	assert len(iq) == 1
	assert iq.dequeue() == (1,111)
	assert len(iq) == 0

