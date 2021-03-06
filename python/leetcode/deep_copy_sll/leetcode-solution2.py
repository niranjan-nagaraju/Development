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

		x = head
		while x:
			x_ = RandomListNode(x.label)

			# Insert x_ next to x
			x_.next = x.next
			x.next = x_

			x = x_.next # skip to next node in the original list

		copy_head = head.next

		# Now link the copy list's and random links 
		x = head
		while x:
			x_ = x.next

			# Not all nodes need to have a valid random link
			x_.random = x.random.next if x.random else None
			
			x = x_.next

		# Once all the random links have been established, Untangle the consolidated list into two
		# by plucking out every second node into the copy SLL
		# restoring the original list back to where it was.
		x = head
		while x:
			x_ = x.next
			
			x.next = x.next.next # There's atleast one node in the original node
			x_.next = x_.next.next if x_.next else None

			x = x.next

		return copy_head


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

