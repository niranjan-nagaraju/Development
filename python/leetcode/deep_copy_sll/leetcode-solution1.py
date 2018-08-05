# Definition for singly-linked list with a random pointer.
class RandomListNode(object):
	def __init__(self, x):
		self.label = x
		self.next = None
		self.random = None

class Solution(object):
	def copyRandomList(self, head):
		"""
		:type head: RandomListNode
		:rtype: RandomListNode
		"""
		if not head:
			return None

		nodesAssocTable = {}

		# Create copy of all the nodes
		# while keeping a table of original -> copy nodes associations
		x = head
		while x:
			x_ = RandomListNode(x.label)
			nodesAssocTable[x] = x_
			x = x.next

		# Now link the copy list's nodes' next and random links 
		x = head
		while x:
			x_ = self.safeLookup(nodesAssocTable, x)
			x_.next = self.safeLookup(nodesAssocTable, x.next)
			x_.random = self.safeLookup(nodesAssocTable, x.random)

			x = x.next

		copy_head = nodesAssocTable[head]

		return copy_head

	def safeLookup(self, table, key):
		try:
			return table[key]
		except KeyError:
			return None

	def compare(self, one, other):
		x = one
		y = other

		while x:
			try:
				assert(x.label == y.label)
				if (x.next):
					assert(x.next.label == y.next.label)
				if (x.random):
					assert(x.random.label == y.random.label)
			except AssertionError as e:
				return False

			x = x.next
			y = y.next

		return True

def TC1():
	a = RandomListNode(1) 
	b = RandomListNode(2) 
	c = RandomListNode(3) 
	d = RandomListNode(4) 
	e = RandomListNode(5) 
	a.next = b
	b.next = c
	c.next = d
	d.next = e

	a.random = e
	b.random = d
	c.random = c
	d.random = b
	e.random = a

	s = Solution()
	copy = s.copyRandomList(a)
	assert(s.compare(a, copy))


def TC2():
	a = RandomListNode(1) 
	b = RandomListNode(2) 
	c = RandomListNode(3) 
	d = RandomListNode(4) 
	e = RandomListNode(5) 
	f = RandomListNode(6) 

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

	s = Solution()
	copy = s.copyRandomList(a)
	assert(s.compare(a, copy))



if __name__ == "__main__":
	TC1()
	TC2()

