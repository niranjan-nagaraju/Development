'''
Solution 1:
===========
Complexity:
	Time: O(n)
	Space: O(n)  {not counting the space needed for copies of the nodes}
Approach:
	For every node, a, in the SLL, Create a copy of the node a' but also store this association
	from a -> a' so anytime we are linking any two random nodes, a-> random = b, we can quickly link
	a'->random = b' using the association.
'''


class SLLNodeWithRandom(object):
	def __init__(self, value, next=None, random=None):
		self.value = value
		self.next = next
		self.random = random

class SLLWithRandom(object):
	def __init__(self):
		self.head = None
		self.len = 0


	def __str__(self):
		x = self.head
		sll_str = "[%d]" %(self.len) 
		while x is not None:
			sll_str += "(%d--%d)-> " %(x.value, x.random.value) 
			x = x.next

		return sll_str


	def __eq__(self, other):
		if (self.len != other.len):
			return False

		x = self.head
		y = other.head

		while x:
			try:
				assert(x.value == y.value)
				if (x.next):
					assert(x.next.value == y.next.value)
				if (x.random):
					assert(x.random.value == y.random.value)
			except AssertionError as e:
				return False

			x = x.next
			y = y.next

		return True


	def safeLookup(self, table, key):
		try:
			return table[key]
		except KeyError:
			return None


	def deepcopy(self):
		if not self.head:
			return None

		copy = SLLWithRandom()
		nodesAssocTable = {}

		# Create copy of all the nodes
		# while keeping a table of original -> copy nodes associations
		x = self.head
		while x:
			x_ = SLLNodeWithRandom(x.value)
			nodesAssocTable[x] = x_
			x = x.next

		# Now link the copy list's nodes' next and random links 
		x = self.head
		while x:
			x_ = self.safeLookup(nodesAssocTable, x)
			x_.next = self.safeLookup(nodesAssocTable, x.next)
			x_.random = self.safeLookup(nodesAssocTable, x.random)

			x = x.next

		copy.head = nodesAssocTable[self.head]
		copy.len = self.len

		return copy



def TC1():
	a = SLLNodeWithRandom(1) 
	b = SLLNodeWithRandom(2) 
	c = SLLNodeWithRandom(3) 
	d = SLLNodeWithRandom(4) 
	e = SLLNodeWithRandom(5) 
	a.next = b
	b.next = c
	c.next = d
	d.next = e

	a.random = e
	b.random = d
	c.random = c
	d.random = b
	e.random = a

	sll = SLLWithRandom()
	sll.head = a
	sll.len = 5

	sll2 = sll.deepcopy()

	print sll
	print sll2

	assert(sll == sll2)
	assert(sll is not sll2)


def TC2():
	a = SLLNodeWithRandom(1) 
	b = SLLNodeWithRandom(2) 
	c = SLLNodeWithRandom(3) 
	d = SLLNodeWithRandom(4) 
	e = SLLNodeWithRandom(5) 
	f = SLLNodeWithRandom(6) 

	a.next = b
	b.next = c
	c.next = d
	d.next = e
	e.next = f

	a.random = d
	b.random = c
	c.random = e
	d.random = a
	e.random = f
	f.random = b

	sll = SLLWithRandom()
	sll.head = a
	sll.len = 6

	sll2 = sll.deepcopy()

	print sll
	print sll2

	assert(sll == sll2)
	assert(sll is not sll2)


if __name__ == "__main__":
	TC1()
	TC2()

