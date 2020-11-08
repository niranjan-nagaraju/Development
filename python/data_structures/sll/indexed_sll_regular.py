'''
An Indexed SLL implemented using a regular SLL.

IndexedSLL:
	- push_back(key, value): push an item(key, value) to the back of the SLL. {O(1) time}
	- pop_front(): Remove item(key, value) from the front of the SLL and return it. {O(1) time}
	- get(key): return value corresponding to matching key in the SLL. {O(1) time}
	- remove(key): remove item(key,value) matching key from the SLL, {O(1) time}*
				 except when item is the tail of the SLL which will take O(n) time.
				 An LRU-cache, for e.g,  does not need to remove its tail node to work.

			 PS: if your usage/Indexed-Queue implementation needs the tail node to be removed in O(1),
				 Please see, Indexed SLL using an SLL with tail-sentinel.
'''

class IndexedSLL(object):
	class Node(object):
		def __init__(self, item=None):
			self.item = item
			self.next = None

		def __str__(self):
			return str(self.item)

	def __init__(self):
		# Initialize a sentinel head so enqueue/dequeue needn't check (for head==None)
		# slightly optimizes inserts/deletes
		self.head = self.tail = IndexedSLL.Node()
		self.lookup = {}
		self.num_items = 0
	

	def __str__(self):
		tmp = self.head
		qstr = []
		while tmp:
			qstr.append(str(tmp))
			tmp = tmp.next

		dstr = []
		for k,v in self.lookup.items():
			dstr.append(str(k) + ":" + str(v))
		return str(qstr) + '\n' + str(dstr)


	# Number of items in the SLL
	def __len__(self):
		return self.num_items


	# Return the value corresponding to key
	# in the SLL
	# None if the key does not exist in the SLL
	def get(self, key):
		node = self.lookup.get(key)
		return node.item[1] if node is not None else None


	# Add 'key' to the back of the SLL
	def push_back(self, key, value):
		node = IndexedSLL.Node((key, value))
		
		self.tail.next = node
		self.tail = node

		self.lookup[key] = node
		self.num_items += 1



	# Remove 'key' from the front of the SLL and lookup table
	def pop_front(self):
		if self.num_items == 0:
			raise ValueError("IndexedSLL is empty.")

		first = self.head.next
		item = first.item
	
		self.num_items-= 1
		self.head.next = first.next

		# popped node is the last node in the SLL
		# update tail to point back to sentinel head-node
		if self.tail == first:
			self.tail = self.head

		self.lookup[first.item[0]] = None
		return item


	# Remove a 'key' from the SLL
	#  If key is at the tail of the SLL, remove() will take O(n) time.
	#
	# *UNSAFE* :
	#	doesn't check if the key is present in the SLL
	# Caller is expected to do these checks before calling
	def remove(self, key):
		node = self.lookup.get(key)

		# prepare for remove
		self.num_items-= 1
		self.lookup[key] = None

		if node is self.tail:
			# `key` is part of the SLL's tail node
			# traverse to penultimate node
			# and remove the tail node
			trav = self.head.next
			while trav.next is not self.tail:
				trav = trav.next
			trav.next = None	
			self.tail = trav
			return


		# Yank node's next from the SLL
		# copy node.next item into node
		# and remove node.next from the list
		item = node.item
		yanked = node.next
		node.item = yanked.item
		node.next = yanked.next

		yanked.next = None

		if yanked == self.tail:
			# yanked node was tail
			# update tail to previous node
			self.tail = node

		# node that previously contained item(to delete)
		# now contains next node's item
		# update lookup table
		self.lookup[node.item[0]] = node




if __name__ == '__main__':
	isll = IndexedSLL()
	assert isll.num_items == 0
	assert (not isll) == True

	isll.push_back(1,5)
	isll.push_back(2,10)
	isll.push_back(3,15)
	assert isll.num_items == 3

	assert isll.pop_front() == (1,5)
	assert isll.num_items == 2
	assert isll.lookup.get(1) == None
	assert isll.lookup.get(2) == isll.head.next
	assert isll.lookup.get(3) == isll.head.next.next
	assert isll.get(1) == None
	assert isll.get(2) == 10
	assert isll.get(3) == 15

	isll.remove(2)
	assert isll.num_items == 1
	assert isll.lookup.get(2) == None
	assert isll.lookup.get(3) == isll.head.next
	assert isll.get(2) == None
	assert isll.get(3) == 15

	isll.push_back(1, 100)
	assert isll.num_items == 2
	assert isll.get(1) == 100
	assert isll.lookup.get(3) == isll.head.next

	isll.push_back(5, 150)
	assert isll.num_items == 3
	assert isll.get(5) == 150
	assert isll.lookup.get(5) == isll.head.next.next.next
	assert isll.lookup.get(5) is isll.tail

	isll.remove(5)
	assert isll.num_items == 2
	assert isll.get(5) == None
	assert isll.tail.item == (1,100)
	assert isll.head.next.item == (3,15)

	isll.push_back(6, 'six')
	assert isll.num_items == 3
	assert isll.get(6) == 'six'

	# SLL: 3 -> 1 -> 6
	assert isll.lookup.get(1) == isll.head.next.next
	assert isll.tail is not isll.lookup.get(1)
	isll.remove(1)
	assert isll.num_items == 2
	assert isll.get(1) == None

	assert str(isll).split('\n')[0] == \
			"['None', '(3, 15)', \"(6, 'six')\"]"

	assert isll.pop_front() == (3,15)
	assert isll.num_items == len(isll) == 1
	assert isll.get(3) == None

	assert isll.pop_front() == (6, 'six')
	assert isll.num_items == len(isll) == 0
	assert isll.get(6) == None

