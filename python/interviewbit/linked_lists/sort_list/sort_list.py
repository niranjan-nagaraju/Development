'''
https://www.interviewbit.com/problems/sort-list/

Sort List

Sort a linked list in O(n log n) time using constant space complexity.

Example :
	Input : 1 -> 5 -> 4 -> 3
	Returned list : 1 -> 3 -> 4 -> 5
'''


'''
Solution Outline:
	0. Use Merge-sort
	1. Recursively
		1.1 split the list into two using fast(2x) and slow(1x) pointers
			Until the list size is either 1 or 2.
		1.2 If list size is 2, and head > head.next, swap nodes
		1.3 Merge the two sorted halves


Sample run:
	5 -> 2 -> 3 -> 1 -> 6 -> 7 -> 4

	split():
		Use fast,slow to get to middle node (1)
		split the list into two,
			l1: 5 -> 2 -> 3
			l2: 1 -> 6 -> 7 -> 4
			split(l1):
				l1: 5
				l2: 2 -> 3
				split(l1): 5
				split(l2): 2 -> 3
			Merge:
				2 -> 3 -> 5
			split(l2):
				l1: 1 -> 6
				l2: 7 -> 4
				split(l1): 1 -> 6
				split(l2): 4 -> 7
			Merge:
				1 -> 4 -> 6 -> 7
		Merge:
			1 -> 2 -> 3 -> 4 -> 6 -> 7

Complexity: O(n log n)
	Split(): nlogn
	Merge(): nlogn
	== O(2nlogn) == O(nlogn)
'''

class ListNode:
	def __init__(self, val):
		self.val = val
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
	# Split list into two halves
	# If the number of nodes in the list is odd,
	# Left-half contains one-less node than the right-half
	def split_list(self, l):
		if not l.next.next:
			l2 = l.next
			l.next = None
			return (l, l2)

		slow = l
		fast = l

		mid = None	
		while fast and fast.next:
			mid = slow
			slow = slow.next
			fast = fast.next.next

		mid.next = None
		return (l, slow)


	# Merge two sorted two list l1, l2 into one sorted list
	# and return its head
	def merge(self, l1, l2):
		l = ListNode(None) # dummy head
		tail = l
		while l1 and l2:
			if l1.val <= l2.val:
				tail.next = l1
				l1 = l1.next
			else:
				tail.next = l2
				l2 = l2.next
			tail = tail.next
			tail.next = None

		if l1:
			tail.next = l1
		elif l2:
			tail.next = l2

		return l.next


	# Sort the linked list using merge sort
	def sort_list(self, A):
		if not A or not A.next:
			return A

		l, r = self.split_list(A)

		l = self.sort_list(l)
		r = self.sort_list(r)

		return self.merge(l, r)


if __name__ == '__main__':
	s = Solution()
	l, r = s.split_list(ListNode.fromList([1,2,3,4,5]))
	assert l.toList() == [1,2]
	assert r.toList() == [3,4,5]

	l, r = s.split_list(ListNode.fromList([1,2,3,4]))
	assert l.toList() == [1,2]
	assert r.toList() == [3,4]

	assert s.merge(
			ListNode.fromList([4,6,7]),
			ListNode.fromList([5,7,8,10])
			).toList() == [4,5,6,7,7,8,10]

	assert s.sort_list(ListNode.fromList([5,1,4,3,6,7,2])).toList() == range(1,8)
	assert s.sort_list(ListNode.fromList([5,1,4,8,3,6,7,2])).toList() == range(1,9)
	assert s.sort_list(ListNode.fromList([5,1])).toList() == [1,5]
	assert s.sort_list(ListNode.fromList([5])).toList() == [5]
	assert s.sort_list(ListNode.fromList([5,1,0])).toList() == [0,1,5]

