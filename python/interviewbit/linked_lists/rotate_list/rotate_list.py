'''
https://www.interviewbit.com/problems/rotate-list/

Rotate List

Given a list, rotate the list to the right by k places, where k is non-negative.

For example:
	Given 1->2->3->4->5->NULL and k = 2,
	return 4->5->1->2->3->NULL.
'''


'''
Solution Outline:
	0. Let n be the length of the list, L
	1. if k < n
		1.1.1 Yank out rightmost 'k' nodes into Lr
		1.1.2 Append L to to the right of Lr
				return Lr
			NOTE: Lr's tail would be L's tail before 1.1.1
					So recording L's tail will help appending L to Lr in O(1)
	2. Otherwise if k > n
		e.g., A: 1 2 3 4 5 6 7
			  k: 9
			  rotating 'n=7' times, will yield 'A' back.
			  rotating +2 times after that == rotating by 9
				=> 6 7 1 2 3 4 5
		Perform 1. with (k = k%n)
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
	def rotate_list(self, A, k):
		if not A:
			return A

		n = 0
		trav = A
		tail = trav
		while trav:
			tail = trav
			trav = trav.next
			n += 1

		k = (k % n)
		if k == 0:
			return A

		trav = A
		for _ in xrange(n-k-1):
			# Traverse (n-k) nodes
			# so there are 'k' nodes to the right of trav
			# when the loop ends
			trav = trav.next

		# Yank last k nodes
		rightK = trav.next
		trav.next = None

		# rightK's tail == original A's tail
		tail.next = A
		return rightK


if __name__ == '__main__':
	s = Solution()
	assert s.rotate_list(ListNode.fromList([1,2,3,4,5]), 2).toList() == [4,5,1,2,3]
	assert s.rotate_list(ListNode.fromList([1,2,3,4,5]), 7).toList() == [4,5,1,2,3]
	assert s.rotate_list(ListNode.fromList([1,2,3,4,5]), 10).toList() == [1,2,3,4,5]
	assert s.rotate_list(ListNode.fromList([1,2,3,4,5,6,7]), 3).toList() == [5,6,7,1,2,3,4]
