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
	'''
	Checks if a SLL has a loop/cycle or not
	using 2 pointers, one fast pointer moving 2 nodes at a time
	and another slow moving pointer moving 1 node at a time
	O(1) memory + O(n) time

	To identify the start of the cycle,
	  Assume the start of the cycle is 'x' nodes away from head.
	  Let's say the loop is made of 'm' nodes.
	  (This means the number of nodes in the SLL is (x+m-1), discounting the start node which is counted twice in both x and m.)
	  When the slow pointer reaches this node 'x', the fast pointer would already have
	  made certain trips inside the loop.
	  The fast pointer has made a certain number of loops, say 'l' and is at 'k' nodes ahead of starting node 'x'
                                   [c1] -> [c2] -> . . . [k] 
                                 /                        \   
       [0] -> [1] -> . . . -> [x]                          [ci]
                                 \                        /
                                  [cm]  <-  . . .   <-  [cj]

	Therefore,
	x = ml + k (by definition)

	From [x], both slow and fast pointer would meet at say 'c' nodes away from [x] after 'r' loops
	Hence,
	(rm + c)%m === ((2rm+2c)+k) % m   (since fast pointer has a head start of +k nodes)
	c % m == (2c+k) % m
	-k % m == c % m
	=> c = -k {or m-k, 2m-k, ...}

	So, both pointers meet at -k nodes from [x]

	Since x = ml+k,
	If we now start two pointers both moving at a pace of 1 node at a time, one from the start of the list,
	and the other at -k inside the loop
	they'll both meet at [x], since -k trails [x] by 'k' nodes, and x = ml+k


	Example:
	   x = 17
	   m = 10
	   When slow ptr reaches node 17, fast pointer would have traversed 17+17 nodes, or 17 nodes outside the loop, followed by 17 inside.
	   And therefore will be 7 nodes ahead of node 17 (the start of the cycle).

	   From here, they'll both meet at k == 7 nodes behind node 17 inside the loop / +3 nodes ahead of node 17.
	   slow ptr at 0, fast ptr at a headstart of +7
	   if they both move 3 times, slow ptr will be at 7 nodes behind loopstart.
	   fastptr will have moved 6 nodes in the meantime, and with the headstart of 7, would have covered 13 nodes inside the loop => 
	   will be 7 nodes behind from the loopstart.

	   Now if we set off both pointers at the pace of 1 node at a time, ptr2 from -7 node from node 17, ptr1 from head
	   when ptr1 reaches [x = node 17], ptr2 would have made 1 loop and be back at [x = node 17]
	'''
	def detectCycle(self, head):
		"""
		:type head: ListNode
		:rtype: bool
		"""
		fast = head
		slow = head
		try:
			while True:
				fast = fast.next.next
				slow = slow.next

				# Fast and slow pointers eventually caught up with each other
				# => There's a loop
				if fast == slow:
					break

			# At this point, fast and slow pointer are at the same node, 'k' nodes away from the start
			slow = head
			while slow != fast:
				slow = slow.next
				fast = fast.next

			return slow
		except AttributeError:
			# The fast pointer hit a None node
			# => there's no cycle
			return None


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

