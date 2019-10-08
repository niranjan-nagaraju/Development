'''
https://leetcode.com/problems/linked-list-cycle/
Given a linked list, determine if it has a cycle in it.

To represent a cycle in the given linked list, we use an integer pos which represents the position (0-indexed) in the linked list where tail connects to. If pos is -1, then there is no cycle in the linked list.


Example 1:
	Input: head = [3,2,0,-4], pos = 1
	Output: true
	Explanation: There is a cycle in the linked list, where tail connects to the second node.
	3 -> 2 -> 0 -> -4
	     ^  <-  <- /


Example 2:
	Input: head = [1,2], pos = 0
	Output: true
	Explanation: There is a cycle in the linked list, where tail connects to the first node.
	1 -> 2
	^ <- / 



https://leetcode.com/problems/linked-list-cycle_ii/
Given a linked list, return the node where the cycle begins. If there is no cycle, return null.
To represent a cycle in the given linked list, we use an integer pos which represents the position (0-indexed) in the linked list where tail connects to. If pos is -1, then there is no cycle in the linked list.
Note: Do not modify the linked list.

Example 1:
	Input: head = [3,2,0,-4], pos = 1
	Output: node(2)
	Explanation: There is a cycle in the linked list, where tail connects to the second node.
	3 -> 2 -> 0 -> -4
	     ^  <-  <- /


Example 2:
	Input: head = [1,2], pos = 0
	Output: node(1)
	Explanation: There is a cycle in the linked list, where tail connects to the first node.
	1 -> 2
	^ <- / 
'''

from node import Node
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

		head = Node(array[0])
		trav = head
		for i in xrange(1, len(array)):
			trav.next = Node(array[i])
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



	# Checks if a cycle has a loop/cycle or not
	# using a hash table to compare individual nodes
	# O(n) memory + O(n) time 
	def hasCycle_1(self):
		nodes = set()
		trav = self.head
		while trav:
			# found a node linking back to
			# one of the nodes already visited
			if trav in nodes:
				return True
			nodes.add(trav)
			trav = trav.next

		return False


	# Checks if a cycle has a loop/cycle or not
	# using 2 pointers, one fast pointer moving 2 nodes at a time
	# and another slow moving pointer moving 1 node at a time
	# O(1) memory + O(n) time 
	def hasCycle_2(self):
		fast = self.head
		slow = self.head
		try:
			while True:
				fast = fast.next.next
				slow = slow.next

				# Fast and slow pointers eventually caught up with each other
				# => There's a loop
				if fast == slow:
					return True

		except AttributeError:
			# The fast pointer hit a None node
			# => there's no cycle
			return False

		return False




if __name__ == '__main__':
	s1 = SLL_cycle.build([1,2,3,4,5], 2)
	assert s1.head.value == 1
	assert s1.tail.value == 5
	assert s1.tail.next.value == 3
	assert s1.hasCycle_1() == True
	assert s1.hasCycle_2() == True

	s2 = SLL_cycle.build([1], 0)
	assert s2.head.value == 1
	assert s2.tail.value == 1
	assert s2.tail.next.value == 1
	assert s2.hasCycle_1() == True
	assert s2.hasCycle_2() == True

	s3 = SLL_cycle.build([1,2,3,4,5,6], 1)
	assert s3.head.value == 1
	assert s3.tail.value == 6
	assert s3.tail.next.value == 2
	assert s3.hasCycle_1() == True
	assert s3.hasCycle_2() == True

	# same as above, but cyclic link wont be established as pos > size
	s3 = SLL_cycle.build([1,2,3,4,5,6], 100)
	assert s3.head.value == 1
	assert s3.tail.value == 6
	assert s3.tail.next == None
	assert s3.hasCycle_1() == False
	assert s3.hasCycle_2() == False


	s4 = SLL_cycle.build([1,2,3,4,5,6])
	assert s4.head.value == 1
	assert s4.tail.value == 6
	assert s4.tail.next == None
	assert s4.hasCycle_1() == False
	assert s4.hasCycle_2() == False

	s5 = SLL_cycle.build([3,2,0,-4], 1) 
	assert s5.head.value == 3
	assert s5.tail.value == -4
	assert s5.tail.next.value == 2
	assert s5.hasCycle_1() == True
	assert s5.hasCycle_2() == True

	s6 = SLL_cycle.build([1,2], 0) 
	assert s6.head.value == 1
	assert s6.tail.value == 2 
	assert s6.tail.next.value == 1
	assert s6.hasCycle_1() == True
	assert s6.hasCycle_2() == True

	s7 = SLL_cycle.build([1])
	assert s7.head.value == 1
	assert s7.tail.value == 1
	assert s7.tail.next == None
	assert s7.hasCycle_1() == False
	assert s7.hasCycle_2() == False

