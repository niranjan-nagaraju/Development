'''
https://www.interviewbit.com/problems/reverse-link-list-recursion/

Reverse a linked list using recursion.

Example :
Given 1->2->3->4->5->NULL,

return 5->4->3->2->1->NULL.
'''



'''
Solution Outline:
	Use the call stack to store the SLL nodes in reverse order
	reverse the links on each stack unwind
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

		
	def __str__(self):
		return str(self.val)



class Solution:
	# @param A : head node of linked list
	# @return the head node in the linked list
	def reverseList(self, head):
		def reverse_(node):
			if not node.next:
				new_head[0] = node
				return node

			reverse_(node.next).next = node
			return node

		if not head:
			return head

		new_head = [None]
		reverse_(head).next = None
		return new_head[0]


if __name__ == '__main__':
	s = Solution()
	ll = ListNode.fromList([1,2,3,4,5])
	ll = s.reverseList(ll)
	assert ll.toList() == [5,4,3,2,1]

