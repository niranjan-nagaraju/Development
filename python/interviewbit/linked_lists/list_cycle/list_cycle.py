'''
https://www.interviewbit.com/problems/list-cycle/

List Cycle

Given a linked list, return the node where the cycle begins. If there is no cycle, return null.

Try solving it using constant additional space.

Example :
Input : 
                  ______
                 |     |
                 \/    |
        1 -> 2 -> 3 -> 4

Return the node corresponding to node 3. 
'''

'''
Solution Outline:
	0. Let 'n' be the number of nodes (relative to head) at which the cycle begins.
		Let 'c' be the number of nodes in cycle.
		NOTE: The node where the cycle begins is common to both 'n' and 'c'
	1. Start two pointers from head, one 'slow' that traverses 1 node at a time,
		another 'fast' that traverses 2 nodes at a time.
		1.1 When 'slow' reaches the start of the cycle, 'fast' is already pulled into the cycle
			and has covered 'm' cycles and is currently +x nodes ahead from the start of the cycle.
		1.2 'slow' and 'fast' will meet at '-x' nodes from the start of the cycle.
			How?
			'slow' moves (c-x) nodes starting at 0,
			'fast' moves 2*(c-x) nodes in the same time, => 2c-2x == -2x, and the headstart of +x would mean
			both 'slow' and 'fast' also lands at -x nodes.
	2. Once the two pointers meet, 
		2.1 we know when 'slow' reached 'n' nodes, 'fast' had already covered (n + mc+x) nodes
			=> since 'fast' covers exactly 2x nodes as 'slow'
			2*n = n + mc+x
			n = mc+x
			or n === x mod c
		2.2 Therefore, if we start one of the pointers, say 'slow' back from the head,
			and let 'fast' resume traversal but at 1 node at a time now
			They'll eventually meet after 'n' nodes (since 'fast' is now x nodes behind the node where the cycle begins)

Sample run:
	n = 11
	c = 7

	1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 10 -> C0  <-  C6  <-  C5 <-\
														\-> C1 -> C2 -> C3 -> C4

	when 'slow' is at '5', 'fast' is at '10'
    slow: 6, fast: C1
	slow: 7, fast: C3
	slow: 8, fast: C5
	slow: 9, fast: C0
	slow: 10, fast: C2
	slow: C0, fast: C4
	=> x = 4

	Now after (7-4) == 3 nodes, both slow and fast meet at C3 == (-4 nodes away from C0)
	slow: C1, fast: C6
	slow: C2, fast: C1
	slow: C3, fast: C3

	n = 11, c = 7
	m = 1, (number of cycles 'fast' did after getting sucked into the cycle)
	x = 4
	n = mc+x => n == 7+4 == 11

	Now, if we start 'slow' back from head, and reduce 'fast' traversal rate so both move 1 node at a time
	when 'slow' and 'fast' meet, they will meet at C0
	slow: 1 , fast: C4
	slow: 2 , fast: C5
	slow: 3 , fast: C6
	slow: 4 , fast: C0
	slow: 5 , fast: C1
	slow: 6 , fast: C2
	slow: 7 , fast: C3
	slow: 8 , fast: C4
	slow: 9 , fast: C5
	slow: 10, fast: C6
	slow: C0, fast: C0
'''

# Definition for singly-linked list.
class ListNode:
	def __init__(self, x):
		self.val = x
		self.next = None


class Solution:
	def find_start_of_cycle(self, head):
		slow = fast = head
		while slow and fast:
			slow = slow.next

			# Linked list has no cycle
			if not fast.next or not fast.next.next:
				return None

			fast = fast.next.next
	
			# slow and fast meet at -x nodes from the cycle start
			if slow == fast:
				break

		# Move 'slow' back to head
		# and traverse both pointers at the same rate
		# so they meet at the cycle start
		slow = head
		while slow != fast:
			slow = slow.next
			fast = fast.next

		return slow


if __name__ == '__main__':
	s = Solution()
	n1 = ListNode(1)
	n2 = ListNode(2)
	n1.next = n2
	n3 = ListNode(3)
	n2.next = n3
	n4 = ListNode(4)
	n4.next = n3
	n3.next = n4
	
	node = s.find_start_of_cycle(n1)
	assert node.val == 3
	assert node == n3

