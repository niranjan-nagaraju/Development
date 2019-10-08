class ListNode(object):
	 def __init__(self, x):
		 self.val = x
		 self.next = None


class SLL_cycle(object):
	def __init__(self, head=None, tail=None):
		self.head = head
		self.tail = tail


	# Build an SLL from array
	# if pos is mentioned,
	#  Point back SLL's tail to the node 'pos' nodes away from head
	# NOTE: if pos > length of the SLL, then the link from tail will remain empty,
	# and therefore the SLL without cycles
	@staticmethod
	def build(array, pos=None):
		if not array:
			return None

		head = ListNode(array[0])
		trav = head
		for i in xrange(1, len(array)):
			trav.next = ListNode(array[i])
			trav = trav.next

		tail = trav

		if pos is not None:
			trav = head
			try:
				for i in xrange(pos):
					trav = trav.next
				tail.next = trav
			except:
				pass

		return SLL_cycle(head, tail)

if __name__ == '__main__':
	s1 = SLL_cycle.build([1,2,3,4,5], 2)
	assert s1.head.val == 1
	assert s1.tail.val == 5
	assert s1.tail.next.val == 3

	s2 = SLL_cycle.build([1], 0)
	assert s2.head.val == 1
	assert s2.tail.val == 1
	assert s2.tail.next.val == 1

	s3 = SLL_cycle.build([1,2,3,4,5,6], 1)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next.val == 2

	# same as above, but cyclic link wont be established as pos > size
	s3 = SLL_cycle.build([1,2,3,4,5,6], 100)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next == None


	s4 = SLL_cycle.build([1,2,3,4,5,6])
	assert s4.head.val == 1
	assert s4.tail.val == 6
	assert s4.tail.next == None

	s5 = SLL_cycle.build([3,2,0,-4], 1) 
	assert s5.head.val == 3
	assert s5.tail.val == -4
	assert s5.tail.next.val == 2

	s6 = SLL_cycle.build([1,2], 0) 
	assert s6.head.val == 1
	assert s6.tail.val == 2 
	assert s6.tail.next.val == 1

	s7 = SLL_cycle.build([1])
	assert s7.head.val == 1
	assert s7.tail.val == 1
	assert s7.tail.next == None


