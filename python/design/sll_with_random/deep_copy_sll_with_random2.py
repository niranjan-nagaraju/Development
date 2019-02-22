'''
Solution 2:
==========
Complexity:
	Time: O(n)
	Space: O(1)  {not counting the space needed for copies of the nodes}
Approach:
	A hashtable is needed in solution 1 to store the association from a -> a'
	but what if the association is predictable? That would obviate the need for the extra storage.

	what if a' is a->next?
	Start with duplicating each node and adding the duplicate next to the original node, so the list becomes
	a->a'->b->b'-> ...
	With such an arrangement, if a->random = b, a'->random == a->next->random = b->next
	Once all the necessary links are setup for every (a'->next, a->random),
	remove every second node to form its own SLL -> the copy
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
		sll_str = "[%d]: " %(self.len) 
		while x is not None:
			sll_str += "(%s--%s)-> " %(x.value, x.random.value if x.random else None)
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


	def deepcopy(self):
		if not self.head:
			return None

		copy = SLLWithRandom()

		# Create copy of all the nodes
		# while keeping a table of original -> copy nodes associations
		x = self.head
		while x:
			x_ = SLLNodeWithRandom(x.value)

			# Insert x_ next to x
			x_.next = x.next
			x.next = x_

			x = x_.next # skip to next node in the original list

		copy.head = self.head.next
		copy.len = self.len

		# Now link the copy list's and random links 
		x = self.head
		while x:
			x_ = x.next

			# Not all nodes need to have a valid random link
			x_.random = x.random.next if x.random else None
			
			x = x_.next

		# Once all the random links have been established, Untangle the consolidated list into two
		# by plucking out every second node into the copy SLL
		# restoring the original list back to where it was.
		x = self.head
		while x:
			x_ = x.next
			
			x.next = x.next.next # There's atleast one node in the original node
			x_.next = x_.next.next if x_.next else None

			x = x.next

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
	#e.random = f
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

