#+encoding: utf-8

'''
https://leetcode.com/problems/reverse-linked-list-ii/

92. Reverse Linked List II

Reverse a linked list from position m to n. Do it in one-pass.

Note: 1 ≤ m ≤ n ≤ length of list.

Example:
	Input: 1->2->3->4->5->NULL, m = 2, n = 4
	Output: 1->4->3->2->5->NULL
'''

# Definition for singly-linked list.
class ListNode:
	def __init__(self, x):
		self.val = x
		self.next = None

	def __str__(self):
		return str(self.val)


class Solution:
	def reverseBetween(self, head, m, n):
		"""
		:type head: ListNode
		:type m: int
		:type n: int
		:rtype: ListNode
		"""

		# Nothing to do, start position = end position
		if m == n:
			return head

		i = 1 # positions start at 1
		tmp = head
		prev = None
		while i < m:
			prev = tmp
			tmp = tmp.next
			i += 1

		#print prev
		return self.reverseNodes(head, prev, n-m)
	

	# Start at beforeStart.next and reverse num_nodes (start+num_nodes => end)
	# readjust links before and after start,end.
	def reverseNodes(self, head, beforeStart, num_nodes):
		start = beforeStart.next if beforeStart else head

		a = start
		b = a.next
		i = 0
		while i < num_nodes:
			c = b.next

			# Make b->a link
			b.next = a

			a,b = b,c
			i += 1

		# end is now at a, end's (earlier) next node is at c (also == b)

		# Adjust end to point to beforeStart, and start to point to end
		start.next = c

		if beforeStart:
			# Reversed a fragment of the list not starting at head
			# head remains unaffected
			# Establish node before starting position to node after ending position
			beforeStart.next = a
		else:
			# reverse started at head node
			# changing head node to node num_nodes away in the original list
			head = a

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

	head = fromList([1,2,3,4,5])
	assert(toList(head) == [1,2,3,4,5])
	#printList(head)
	head = s.reverseBetween(head, 1, 4)
	#printList(head)
	assert(toList(head) == [4,3,2,1,5])

	head = fromList([1,2,3,4,5])
	head = s.reverseBetween(head, 2, 4)
	assert(toList(head) == [1,4,3,2,5])

	head = fromList([1,2,3,4,5])
	head = s.reverseBetween(head, 1, 5)
	assert(toList(head) == [5,4,3,2,1])

