'''
Indexed Singly-Linked List
	- Uses a regular SLL + hash-table
	- remove(tail) is *not* O(1), It's assumed it won't be needed.
'''
class IndexedSLL(object):
	class Node:
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
			return

		first = self.head.next
	
		self.num_items-= 1
		self.head.next = first.next

		# popped node is the last node in the SLL
		# update tail to point back to sentinel head-node
		if self.tail == first:
			self.tail = self.head

		self.lookup[first.item[0]] = None


	# Remove a 'key' from the SLL
	#
	# *UNSAFE* :
	#	doesn't check if the key is present in the SLL
	#	or if the key is in the tail node. 
	# Caller is expected to do these checks before calling
	def remove(self, key):
		node = self.lookup.get(key)
		self.num_items-= 1

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
		self.lookup[key] = None


	# Lookup key's node, and return its value
	# None if the key doesn't exist
	def get(self, key):
		return self.lookup.get(key)



