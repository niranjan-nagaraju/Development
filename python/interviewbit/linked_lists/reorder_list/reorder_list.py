#encoding: utf-8
'''
https://www.interviewbit.com/problems/reorder-list/

Reorder List

Given a singly linked list
    L: L0 → L1 → … → Ln-1 → Ln,

reorder it to:
	L0 → Ln → L1 → Ln-1 → L2 → Ln-2 → …

You must do this in-place without altering the nodes’ values.

For example,
Given {1,2,3,4}, reorder it to {1,4,2,3}.
'''

'''
Solution Outline:
	1. Navigate to the middle of the linked list, L.
	2. Remove all nodes from the middle, till the end and stack them up (push_front) in another linked list, L2.
	3. Interleave elements from L2 into L.

Sample run:
	L: 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
	middle: 4
	
	remove 5 -> 6 -> 7, one node at a time, and adding them to the front of the new linked-list L2
	L2: 5
	L2: 6 -> 5
	L2: 7 -> 6 -> 5

	Interleave L2 into L
	1 -> 7 -> 2 -> 6 -> 3 -> 5 -> 4
'''
# Definition for singly-linked list.
class ListNode:
	def __init__(self, x):
		self.val = x
		self.next = None

	# Build a linked-list from a list
	@staticmethod
	def fromList(l):
		head = ListNode(l[0])
		tmp = head
		for x in l[1:]:
			node = ListNode(x)
			tmp.next = node
			tmp = tmp.next
		return head


	# Build a list from a linked-list
	def toList(self):
		tmp = self
		l = []
		while tmp:
			l.append(tmp.val)
			tmp = tmp.next

		return l


class Solution:
	def reorder_list(self, head):
		# reverse list starting at ll
		# return new head of the reversed sll
		def reverse(ll):
			l2_head = None
			while ll:
				# Remove tmp from the list,
				# and add it to the front of the list
				p = ll.next
				ll.next = l2_head
				l2_head = ll
				ll = p
			return l2_head


		# Add l2's nodes after every alternate node in l1
		def interleave(l2, l1):
			tmp1 = l1
			tmp2 = l2

			while tmp1 and tmp2:
				p1 = tmp1.next
				p2 = tmp2.next
				tmp1.next = tmp2
				tmp2.next = p1
				tmp1 = p1
				tmp2 = p2

	
		if not head:
			return head

		# Use slow (1x) and fast (2x) pointers
		# to reach the middle node
		slow = fast = head
		while slow and fast:
			try:
				prev = slow
				slow = slow.next
				fast = fast.next.next
			except AttributeError:
				break

		mid = slow

		# Cut off the chain at 'mid'
		# and move it to L2
		# reverse L2
		prev.next = None
		l2_head = reverse(mid)

		# interleave l2's nodes into l1
		interleave(l2_head, head)
		return head


if __name__ == '__main__':
	s = Solution()

	head = ListNode.fromList([1,2,3,4,5,6,7])
	s.reorder_list(head)
	assert head.toList() == [1,7,2,6,3,5,4]

	head = ListNode.fromList([1,2,3,4,5,6,7,8])
	new_head = s.reorder_list(head)
	assert head == new_head
	assert head.toList() == [1,8,2,7,3,6,4,5]

	assert s.reorder_list(ListNode.fromList([1])).toList() == [1]
	assert s.reorder_list(ListNode.fromList([1,2])).toList() == [1,2]
	assert s.reorder_list(ListNode.fromList([1,2,3])).toList() == [1,3,2]

