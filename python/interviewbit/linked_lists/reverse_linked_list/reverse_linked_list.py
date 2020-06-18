'''
https://www.interviewbit.com/problems/reverse-linked-list/

Reverse Linked List

Reverse a linked list. Do it in-place and in one-pass.

For example:
	Given 1->2->3->4->5->NULL,
	return 5->4->3->2->1->NULL.
'''


'''
Solution Outline:
	1. Use 3 pointers, a,b,c, initialized to first, second and third nodes respectively
	2. Reverse the link from a->b to b->a
	3. Use c to shift all a,b,c to one node to their right, and repeat step 1
	4. At the end, when b falls off the edge, head of the linked list still points to the second node
	    Set head's link to null.


Sample run:
	A: 1 -> 2 -> 3 -> 4 -> 5 -> None
       n1  n2   n3   n4   n5

	a,b,c = n1, n2, n3

	b->a
	A: 1 <-> 2   3 -> 4 -> 5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n2, n3, n4

	b->a
	A: 1 <-> 2 <- 3    4 -> 5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n3, n4, n5

	b->a
	A: 1 <-> 2 <- 3 <- 4    5 -> None
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n4, n5, None

	b->a
	A: 1 <-> 2 <- 3 <- 4 <- 5
       n1   n2   n3   n4   n5
	a,b,c = b, c, c.next
	      = n5, None, None

	b is None
	set head.next = None
	A:  1 <- 2 <- 3 <- 4 <- 5
       n1   n2   n3   n4   n5
	set A to a = n5

	A: n5 -> n4 -> n3 -> n2 -> n1
	   5  -> 4  ->  3 ->  2 ->  1

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

		
	def __str__(self):
		return str(self.val)


class Solution:
	def reverse_list(self, head):
		# linked list is empty
		# or has only 1 node in it
		if not head or not head.next:
			return head

		a,b = head, head.next
		c = b.next
		while b:
			b.next = a
			a,b = b,c
			c = c.next if c else None

		head.next = None
		head = a
		return head

if __name__ == '__main__':
	s = Solution()
	ll = ListNode.fromList([1,2,3,4,5])
	ll = s.reverse_list(ll)
	assert ll.toList() == [5,4,3,2,1]

