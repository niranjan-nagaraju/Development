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
'''
class Solution(object):
	def hasCycle(self, head):
		"""
		:type head: ListNode
		:rtype: bool
		"""
		# Checks if a cycle has a loop/cycle or not
		# using a hash table to compare individual nodes
		# O(n) memory + O(n) time 
		nodes = set()
		trav = head
		while trav:
			# found a node linking back to
			# one of the nodes already visited
			if trav in nodes:
				return True
			nodes.add(trav)
			trav = trav.next

		return False


if __name__ == '__main__':
	s = Solution()
	from SLL_cycle import SLL_cycle
	s1 = SLL_cycle.build([1,2,3,4,5], 2)
	assert s1.head.val == 1
	assert s1.tail.val == 5
	assert s1.tail.next.val == 3
	assert s.hasCycle(s1.head) == True

	s2 = SLL_cycle.build([1], 0)
	assert s2.head.val == 1
	assert s2.tail.val == 1
	assert s2.tail.next.val == 1
	assert s.hasCycle(s2.head) == True

	s3 = SLL_cycle.build([1,2,3,4,5,6], 1)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next.val == 2
	assert s.hasCycle(s3.head) == True

	# same as above, but cyclic link wont be established as pos > size
	s3 = SLL_cycle.build([1,2,3,4,5,6], 100)
	assert s3.head.val == 1
	assert s3.tail.val == 6
	assert s3.tail.next == None
	assert s.hasCycle(s3.head) == False


	s4 = SLL_cycle.build([1,2,3,4,5,6])
	assert s4.head.val == 1
	assert s4.tail.val == 6
	assert s4.tail.next == None
	assert s.hasCycle(s4.head) == False

	s5 = SLL_cycle.build([3,2,0,-4], 1) 
	assert s5.head.val == 3
	assert s5.tail.val == -4
	assert s5.tail.next.val == 2
	assert s.hasCycle(s5.head) == True

	s6 = SLL_cycle.build([1,2], 0) 
	assert s6.head.val == 1
	assert s6.tail.val == 2 
	assert s6.tail.next.val == 1
	assert s.hasCycle(s6.head) == True

	s7 = SLL_cycle.build([1])
	assert s7.head.val == 1
	assert s7.tail.val == 1
	assert s7.tail.next == None
	assert s.hasCycle(s7.head) == False

