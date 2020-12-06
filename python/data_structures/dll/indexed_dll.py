'''
An Indexed DLL implemented using a regular DLL.

IndexedDLL:
	- push_back(key, value): push an item(key, value) to the back of the DLL. {O(1) time}
	- pop_front(): Remove item(key, value) from the front of the DLL and return it. {O(1) time}
	- get(key): return value corresponding to matching key in the DLL. {O(1) time}
	- remove(key): remove item(key,value) matching key from the DLL, {O(1) time}
'''

from node import Node

class IndexedDLL(object):
	def __init__(self):
		# Initialize a sentinel head so enqueue/dequeue needn't check (for head==None)
		# slightly optimizes inserts/deletes
		self.head = self.tail = Node()
		self.lookup = {}
		self.num_items = 0
	

	def __str__(self):
		tmp = self.head.next
		qstr = []
		while tmp:
			qstr.append(str(tmp))
			tmp = tmp.next

		dstr = []
		for k,v in self.lookup.items():
			dstr.append(str(k) + ":" + str(v))
		return str(qstr) + '\n' + str(dstr)


	# Number of items in the DLL
	def __len__(self):
		return self.num_items


	# Return the value corresponding to key
	# in the DLL
	# None if the key does not exist in the DLL
	def get(self, key):
		node = self.lookup.get(key)
		return node.value[1] if node is not None else None


	# Add 'key' to the back of the DLL
	def push_back(self, key, value):
		node = Node((key, value))
		
		self.tail.next = node
		node.prev = self.tail
		self.tail = node

		self.lookup[key] = node
		self.num_items += 1



	# Remove 'key' from the front of the DLL and lookup table
	def pop_front(self):
		if self.num_items == 0:
			raise ValueError("IndexedDLL is empty.")

		first = self.head.next
		item = first.value
	
		self.num_items-= 1
		self.head.next = first.next

		# popped node is the last node in the DLL
		# update tail to point back to sentinel head-node
		if self.tail is first:
			self.tail = self.head
		else:
			first.next.prev = self.head

		first.next = first.prev = None # sanitize node
		self.lookup[item[0]] = None
		return item


	# Remove a 'key' from the DLL
	def remove(self, key):
		node = self.lookup.get(key)
		
		if node is self.head:
			return self.pop_front()

		# prepare for remove
		self.num_items-= 1
		self.lookup[key] = None

		if node is self.tail:
			self.tail = self.tail.prev
		else:
			# p <-> node <-> n
			p = node.prev
			n = node.next
			p.next = n
			n.prev = p

		node.prev = node.next = None # sanitize node
		return node.value





if __name__ == '__main__':
	iDll = IndexedDLL()
	assert iDll.num_items == 0
	assert (not iDll) == True

	# 1 -> 2 -> 3
	iDll.push_back(1,5)
	iDll.push_back(2,10)
	iDll.push_back(3,15)
	assert iDll.num_items == 3

	assert iDll.pop_front() == (1,5)
	# 2 -> 3
	assert iDll.num_items == 2
	assert iDll.lookup.get(1) == None
	assert iDll.lookup.get(2) == iDll.head.next
	assert iDll.lookup.get(2) == iDll.tail.prev
	assert iDll.lookup.get(3) == iDll.head.next.next
	assert iDll.lookup.get(3) == iDll.tail
	assert iDll.get(1) == None
	assert iDll.get(2) == 10
	assert iDll.get(3) == 15

	iDll.remove(2)
	assert iDll.num_items == 1
	assert iDll.lookup.get(2) == None
	assert iDll.lookup.get(3) == iDll.head.next
	assert iDll.get(2) == None
	assert iDll.get(3) == 15

	iDll.push_back(1, 100)
	# 3 -> 1
	assert iDll.num_items == 2
	assert iDll.get(1) == 100
	assert iDll.lookup.get(3) == iDll.head.next

	iDll.push_back(5, 150)
	# 3 -> 1 -> 5
	assert iDll.num_items == 3
	assert iDll.get(5) == 150
	assert iDll.lookup.get(5) == iDll.head.next.next.next
	assert iDll.lookup.get(5) is iDll.tail
	assert iDll.lookup.get(3) is iDll.head.next
	assert iDll.lookup.get(1) is iDll.head.next.next
	assert iDll.lookup.get(1) is iDll.tail.prev

	iDll.remove(5)
	# 3 -> 1
	assert iDll.num_items == 2
	assert iDll.get(5) == None
	assert iDll.tail.value == (1,100)
	assert iDll.head.next.value == (3,15)

	iDll.push_back(6, 'six')
	# 3 -> 1 -> 6
	assert iDll.num_items == 3
	assert iDll.get(6) == 'six'
	assert iDll.lookup.get(6) is iDll.tail

	assert iDll.lookup.get(1) == iDll.head.next.next
	assert iDll.tail is not iDll.lookup.get(1)
	iDll.remove(1)
	assert iDll.num_items == 2
	assert iDll.get(1) == None

	assert str(iDll).split('\n')[0] == \
			"['(3, 15)', \"(6, 'six')\"]"

	assert iDll.pop_front() == (3,15)
	assert iDll.num_items == len(iDll) == 1
	assert iDll.get(3) == None

	assert iDll.pop_front() == (6, 'six')
	assert iDll.num_items == len(iDll) == 0
	assert iDll.get(6) == None

