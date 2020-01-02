'''
https://leetcode.com/problems/odd-even-linked-list/

328. Odd Even Linked List
Given a singly linked list, group all odd nodes together followed by the even nodes. Please note here we are talking about the node number and not the value in the nodes.

You should try to do it in place. The program should run in O(1) space complexity and O(nodes) time complexity.

Example 1:
	Input: 1->2->3->4->5->NULL
	Output: 1->3->5->2->4->NULL

Example 2:
	Input: 2->1->3->5->6->4->7->NULL
	Output: 2->3->6->7->1->5->4->NULL

Note:
	The relative order inside both the even and odd groups should remain as it was in the input.
	The first node is considered odd, the second node even and so on ...
'''
# Definition for singly-linked list.
class ListNode(object):
	def __init__(self, x):
		self.val = x
		self.next = None

	def __str__(self):
		return str(self.val)


class Solution(object):
	def oddEvenList(self, head):
		"""
		:type head: ListNode
		:rtype: ListNode
		"""

		if not head:
			return

		tmp = head
		even_head = even_tail = None
		tail = None
		while tmp:
			tail = tmp
			even_node = tmp.next

			# Size of the SLL is odd
			# and we are at tail
			# there isnt an even node at the end
			if not even_node:
				break

			# remove the even node from the linked list
			# and start aggregating into a separate list
			# with only even nodes in them
			tmp.next = even_node.next
			even_node.next = None

			if not even_head:
				even_head = even_tail = even_node
			else:
				even_tail.next = even_node
				even_tail = even_node

			tmp = tmp.next

		# At this point, the tail of the original list is registered
		# Append even_head to next of tail
		tail.next = even_head
		return head



# A few helper functions to make testcases easier
def printList(head):
	tmp = head
	while tmp:
		print tmp, " -> ",
		tmp = tmp.next
	print


def fromList(lst):
	head = ListNode(lst[0])
	tail = head
	for x in lst[1:]:
		tail.next = ListNode(x)
		tail = tail.next

	return head


def toList(head):
	tmp = head
	lst = []
	while tmp:
		lst.append(tmp.val)
		tmp = tmp.next

	return lst


if __name__ == '__main__':
	s = Solution()
	l1 = fromList([1,2,3])
	s.oddEvenList(l1)

	l1 = fromList([1,2,3,4,5])
	assert toList(l1) == [1,2,3,4,5]
	s.oddEvenList(l1)
	assert toList(l1) == [1,3,5,2,4]

	l2 = fromList([1,2,3,4,5,6])
	assert toList(l2) == [1,2,3,4,5,6]
	s.oddEvenList(l2)
	assert toList(l2) == [1,3,5,2,4,6]

	l3 = fromList([1])
	s.oddEvenList(l3)
	assert toList(l3) == [1]


