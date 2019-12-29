'''
https://leetcode.com/problems/reverse-linked-list/
206. Reverse Linked List

Reverse a singly linked list.

Example:

	Input: 1->2->3->4->5->NULL
	Output: 5->4->3->2->1->NULL

Follow up:
	A linked list can be reversed either iteratively or recursively. Could you implement both?
'''

# Definition for singly-linked list.
class ListNode(object):
	def __init__(self, x):
		self.val = x
		self.next = None

class Solution(object):
	# Reverse an SLL(iterative version)
	# Consider 3 nodes, x->y->z
	# a,b,c = x,y,z
	# Start with reversing a->b to a<-b,
	# then hop onto y,z,..., with a=y,b=z,... and repeat
	def reverseList(self, head):
		"""
		:type head: ListNode
		:rtype: ListNode
		"""
		a = None
		b = head
		while b:
			c = b.next

			# Make b->a link
			# Also takes care of head, since a is initially None
			# Imagine to the left of head
			b.next = a

			a = b
			b = c

		# When all's done, 'a' is pointing to the 'tail'
		# of the SLL
		# Since we have now reversed, point 'head' to 'a',
		# but before that, Update 'tail' to erstwhile 'head'
		return a



if __name__ == '__main__':
	l = ListNode(1)
	l.next = ListNode(2)
	l.next.next = ListNode(3)
	l.next.next.next = ListNode(4)
	l.next.next.next.next = ListNode(5)

	s = Solution()
	l = s.reverseList(l)

	tmp = l
	lst = []
	while tmp:
		lst.append(tmp.val)
		tmp = tmp.next

	assert(lst == [5,4,3,2,1])

