'''
https://leetcode.com/problems/merge-two-sorted-lists/

21. Merge Two Sorted Lists

Merge two sorted linked lists and return it as a new list. The new list should be made by splicing together the nodes of the first two lists.

Example:

	Input: 1->2->4, 1->3->4
	Output: 1->1->2->3->4->4
'''


'''
Solution Outline:
	Start a new empty LL: l3
	Add to the end of l3 choosing from l1 or l2 whichever is smaller
	In the end, append whatever is left of l1 or l2 into l3
'''

# Definition for singly-linked list.
class ListNode(object):
	def __init__(self, x):
		self.val = x
		self.next = None

class Solution(object):
	def mergeTwoLists(self, l1, l2):
		"""
		:type l1: ListNode
		:type l2: ListNode
		:rtype: ListNode
		"""
		l3_head, l3_tail = None, None
		while l1 and l2:
			if l1.val < l2.val:
				node = l1
				l1 = l1.next
			else:
				node = l2
				l2 = l2.next

			node.next = None # sanitize node
			if not l3_head:
				l3_head = node
			else:
				l3_tail.next = node
			l3_tail = node
			
		# In the end, if either one of l1 or l2 still has elements
		# then they are all > the other list which is now exhausted
		# link whole of l1/l2 to the end
		if l1:
			# l3_tail wont be updated to the real tail
			# but this is OKAY
			# we just need to return the head of l3
			if not l3_head:
				l3_head = l1
			else:
				l3_tail.next = l1

		if l2:
			# l3_tail wont be updated to the real tail
			# but this is OKAY
			# we just need to return the head of l3
			if not l3_head:
				l3_head = l2
			else:
				l3_tail.next = l2

		return l3_head


def fromList(lst):
	if not lst:
		return []

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
	l1 = fromList([1,2,4])
	l2 = fromList([1,3,4])
	l3 = s.mergeTwoLists(l1, l2)
	assert toList(l3) == [1,1,2,3,4,4]

	assert toList(s.mergeTwoLists(fromList([]), fromList([2,4,6,8]))) == [2,4,6,8]
	assert toList(s.mergeTwoLists(fromList([1,3,5,7]), fromList([2,4,6,8]))) == [1,2,3,4,5,6,7,8]

