#encoding: utf-8
'''
https://www.interviewbit.com/problems/intersection-of-linked-lists/

Intersection of Linked Lists

Write a program to find the node at which the intersection of two singly linked lists begins.

For example, the following two linked lists:


A:          a1 → a2
                   ↘
                     c1 → c2 → c3
                   ↗
B:     b1 → b2 → b3

begin to intersect at node c1.

Notes:
1. If the two linked lists have no intersection at all, return null.
2. The linked lists must retain their original structure after the function returns.
3. You may assume there are no cycles anywhere in the entire linked structure.
4. Your code should preferably run in O(n) time and use only O(1) memory.
'''

'''
Solution Outline:
	0. Let nA be the length of linked list A, nB be the length of linked list B
	1. Consider nA > nB
		1.1 Skip (nA-nB) nodes at the start of nA
		1.2 Compare nodes, one at a time in A and B as long as they don't match each other
		    (or until either of A or B run out)
		1.3 The matching node is the intersection of A and B, if either of A and B runs out in 1.2,
		    then the linked lists A and B do not intersect
	2. if nA < nB,
	    Swap A and B, (and nA, nB), and proceed as in 1.
'''
# Definition for singly-linked list.
class ListNode:
	def __init__(self, x):
		self.val = x
		self.next = None


class Solution:
	# @param A : head node of linked list
	# @param B : head node of linked list
	# @return the head node in the linked list
	def getIntersectionNode(self, A, B):
		# Calculate length of A
		nA = 0
		trav = A
		while trav:
			trav = trav.next
			nA += 1

		# Calculate length of B
		nB = 0
		trav = B
		while trav:
			trav = trav.next
			nB += 1

		# A will be considered to be the longer chain
		# if it isn't swap A, B so
		# A will be the longer chain from this point
		if nA < nB:
			A, B = B, A
			nA, nB = nB, nA

		# skip 'diff' nodes at the start of A
		diff = nA-nB
		tA = A
		while diff:
			diff -= 1
			tA = tA.next

		# Traverse nodes in A and B until an intersection
		# is found or we run out of either A or B
		tB = B
		while tA and tB:
			if tA == tB:
				# Found a node common to both A and B
				return tA
			tA = tA.next
			tB = tB.next

		# ran out of A or B or both
		# A, B don't have any intersection node
		return None


if __name__ == '__main__':
	s = Solution()
	A = ListNode(1)
	A.next = ListNode(2)
	A.next.next = ListNode(3)

	B = ListNode(3)
	B.next = ListNode(4)
	
	t = ListNode(6)
	t.next = ListNode(7)

	A.next.next.next = t
	B.next.next = t
	assert s.getIntersectionNode(A, B) == t

	# Break the intersection point of A and B
	A.next.next.next = ListNode(4)
	B.next.next = ListNode(5)
	assert s.getIntersectionNode(A, B) == None

