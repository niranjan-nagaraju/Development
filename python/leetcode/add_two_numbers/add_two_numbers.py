'''
https://leetcode.com/problems/add-two-numbers/solution/

You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.

Example:

	Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
	Output: 7 -> 0 -> 8
	Explanation: 342 + 465 = 807.
'''

# Definition for singly-linked list.
class ListNode(object):
	def __init__(self, x):
		self.val = x
		self.next = None

	@staticmethod
	def fromList(lst):
		s = ListNode(lst[0])
		t = s
		for x in lst[1:]:
			t.next = ListNode(x)
			t = t.next

		return s


	@staticmethod
	def toList(node):
		l = []
		while node:
			l.append(node.val)
			node = node.next

		return l
		


class Solution(object):
	def addTwoNumbers(self, l1, l2):
		"""
		:type l1: ListNode
		:type l2: ListNode
		:rtype: ListNode
		"""

		carry = 0
		s = None
		tmp = None

		# Add last 'x' digits of l1 and l2 until either of them run out of digits
		while l1 and l2:
			added = (carry + l1.val + l2.val) % 10
			carry = (carry + l1.val + l2.val) / 10

			next_digit = ListNode(added)
			if not s:
				s = tmp = next_digit
			else:              
				tmp.next = next_digit
				tmp = tmp.next

			l1 = l1.next
			l2 = l2.next


		# if l2 has run out of digits, then l2's leading digits are zeroes,
		# just that they are not explicity stored in the list l2
		while l1:
			added = (carry + l1.val) % 10
			carry = (carry + l1.val) / 10

			next_digit = ListNode(added)

			# l2 might have been null to begin with
			if not s:
				s = tmp = next_digit
			else:              
				tmp.next = next_digit
				tmp = tmp.next
			l1 = l1.next

		# if l1 has run out of digits, then l1's leading digits are zeroes,
		# just that they are not explicity stored in the list l1
		while l2:
			added = (carry + l2.val) % 10
			carry = (carry + l2.val) / 10

			next_digit = ListNode(added)

			# l1 might have been null to begin with
			if not s:
				s = tmp = next_digit
			else:              
				tmp.next = next_digit
				tmp = next_digit
			l2 = l2.next

		# At the end of adding all digits, if there is a carry
		# Add a new  node for the carry 
		if carry:
			tmp.next = ListNode(1) # if carry exists, it'd be 1

		return s



if __name__ == "__main__":
	sol = Solution()

	# a: 2->4->3
	a = ListNode.fromList([2,4,3])

	# b: 5->6->4
	b = ListNode.fromList([5,6,4])

	assert(ListNode.toList(sol.addTwoNumbers(a, b)) == [7,0,8])

	assert(ListNode.toList(
		sol.addTwoNumbers(
			ListNode.fromList([1,9,9]), 
			ListNode.fromList([9]) ) 
		) == [0,0,0,1])


	assert(ListNode.toList(
		sol.addTwoNumbers(
			ListNode.fromList([9]), 
			ListNode.fromList([1,9,9]) ) 
		) == [0,0,0,1])


	assert(ListNode.toList(
		sol.addTwoNumbers(
			None, 
			ListNode.fromList([1,9,9]) ) 
		) == [1,9,9])


	assert(ListNode.toList(
		sol.addTwoNumbers(
			ListNode.fromList([1,9,9]),
			None )
		) == [1,9,9])

