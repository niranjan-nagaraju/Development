'''
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

import sys
sys.path.append("../linked_list_cycle/")
from SLL_cycle import SLL_cycle


class Solution(object):
	# Checks if a cycle has a loop/cycle or not
	# using a hash table to store a list of previously visited nodes
	# O(n) memory + O(n) time 
	def detectCycle(self, head):
		"""
		:type head: ListNode
		:rtype: bool
		"""
		nodes = set()
		trav = head
		while trav:
			# found a node linking back to
			# one of the nodes already visited
			if trav in nodes:
				return trav
			nodes.add(trav)
			trav = trav.next

		return None


if __name__ == '__main__':
	s = Solution()
	from SLL_cycle import SLL_cycle
	s1 = SLL_cycle.build([1,2,3,4,5], 2)
	assert s1.head.val == 1
	assert s1.tail.val == 5
	assert s1.tail.next.val == 3
	assert s.detectCycle(s1.head) == s1.tail.next

	s2 = SLL_cycle.build([1], 0)
	assert s2.head.val == 1
	assert s2.tail.val == 1
	assert s2.tail.next.val == 1
	assert s.detectCycle(s2.head) == s2.tail.next

	s3 = SLL_cycle.build([1,2,3,4,5,6], 1)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next.val == 2
	assert s.detectCycle(s3.head) == s3.tail.next

	# same as above, but cyclic link wont be established as pos > size
	s3 = SLL_cycle.build([1,2,3,4,5,6], 100)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next == None
	assert s.detectCycle(s3.head) ==  None


	s4 = SLL_cycle.build([1,2,3,4,5,6])
	assert s4.head.val == 1
	assert s4.tail.val == 6
	assert s4.tail.next == None
	assert s.detectCycle(s4.head) == None

	s5 = SLL_cycle.build([3,2,0,-4], 1) 
	assert s5.head.val == 3
	assert s5.tail.val == -4
	assert s5.tail.next.val == 2
	assert s.detectCycle(s5.head) == s5.tail.next

	s6 = SLL_cycle.build([1,2], 0) 
	assert s6.head.val == 1
	assert s6.tail.val == 2 
	assert s6.tail.next.val == 1
	assert s.detectCycle(s6.head) == s6.tail.next

	s7 = SLL_cycle.build([1])
	assert s7.head.val == 1
	assert s7.tail.val == 1
	assert s7.tail.next == None
	assert s.detectCycle(s7.head) == None

	s8 = SLL_cycle.build(range(1, 27), 16)
	assert s8.head.val == 1
	assert s8.tail.val == 26
	assert s8.tail.next.val == 17
	assert s.detectCycle(s8.head) == s8.tail.next

