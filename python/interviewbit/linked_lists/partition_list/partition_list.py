'''
https://www.interviewbit.com/problems/partition-list/

Partition List

Given a linked list and a value x, partition it such that all nodes less than x come before nodes greater than or equal to x.

You should preserve the original relative order of the nodes in each of the two partitions.

For example,
Given 1->4->3->2->5->2 and x = 3,
return 1->2->2->4->3->5.
'''

'''
Solution Outline:
	1. Scan the linked list, L.
	2. Remove and separate nodes with value >= x into its own list, L2.
		2.1. Remove and separate nodes with value < x into its own list, L1
	3. Concatenate L2 to the end of L1 and return L1


Sample run:
	L: 1 -> 4 -> 3 -> 2 -> 5 -> 2
	L1: {}
	L2: {}
	x: 3

	node: 1
	L: 4 -> 3 -> 2 -> 5 -> 2
	L1: 1
	L2: {}

	node: 4	
	L:  3 -> 2 -> 5 -> 2
	L1: 1
	L2: 4

	node: 3	
	L:  2 -> 5 -> 2
	L1: 1
	L2: 4 -> 3

	node: 2	
	L:  5 -> 2
	L1: 1 -> 2
	L2: 4 -> 3

	node: 5	
	L:  2
	L1: 1 -> 2
	L2: 4 -> 3 -> 5

	node: 2
	L: {}
	L1: 1 -> 2 -> 2
	L2: 4 -> 3 -> 5

	L = L1 + L2
	L: 1 -> 2 -> 2 -> 4 -> 3 -> 5
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


# SLL container to hold (head, tail)
# corresponding to a linked list
class SLL:
	def __init__(self, head, tail):
		self.head = head
		self.tail = tail

class Solution:
	def partition_list(self, A, x):
		def moveNodeToLL(ll, node):
			ll.tail.next = node
			ll.tail = node
			node.next = None

		# Initialize L1 with a dummy head so appends are easier
		l1_head = ListNode(None)
		l1 = SLL(l1_head, l1_head)

		# Initialize L2 with a dummy head so appends are easier
		l2_head = ListNode(None)
		l2 = SLL(l2_head, l2_head)

		while A:
			next = A.next
			if A.val >= x:
				moveNodeToLL(l2, A)
			else:
				moveNodeToLL(l1, A)
			A = next
		
		# Append l2's real head to l1's tail
		l1.tail.next = l2.head.next

		# return l1's read head skipping past the dummy head
		return l1.head.next



if __name__ == '__main__':
	s = Solution()
	ll = s.partition_list(ListNode.fromList([4,1,2,3,2,5]), 3)
	assert ll.toList() == [1,2,2,4,3,5]
	ll = s.partition_list(ListNode.fromList([1,4,3,2,5,2]), 3)
	assert ll.toList() == [1,2,2,4,3,5]

	ll = s.partition_list(ListNode.fromList([5,4,6]), 3)
	assert ll.toList() == [5,4,6]

	ll = s.partition_list(ListNode.fromList([1,2]), 3)
	assert ll.toList() == [1,2]

