'''
https://www.interviewbit.com/problems/kth-node-from-middle/

Kth Node From Middle

Problem Description
Given a linked list A of length N and an integer B.
You need to find the value of the Bth node from the middle towards the beginning of the Linked List A.
If no such element exists, then return -1.

NOTE:
	Position of middle node is: (N/2)+1, where N is the total number of nodes in the list.

Problem Constraints
	1 <= N <= 10^5
	1<= Value in Each Link List Node <= 10^3
	1 <= B <= 10^5

Input Format
First argument is the head pointer of the linkedlist A.
Second argument is an integer B.


Output Format
Return an integer denoting the value of the Bth from the middle towards the head of the linked list A. If no such element exists, then return -1.

Example Input
Input 1:
 A = 3 -> 4 -> 7 -> 5 -> 6 -> 1 6 -> 15 -> 61 -> 16
 B = 3
Input 2:
 A = 1 -> 14 -> 6 -> 16 -> 4 -> 10
 B = 2
Input 3:
 A = 1 -> 14 -> 6 -> 16 -> 4 -> 10
 B = 10


Example Output
Output 1:
 4
Output 2:
 14
Output 3:
 -1
'''


'''
Solution Outline:
	0. Use 3 pointers, a, b, c.
	1. `c` moved 2 nodes at a time, `b` moves 1 node at a time, `a` trails `b` by `k` nodes.
	2. return `a` if its not empty at the end of the traversal.
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
	def kth_node_from_mid(self, head, k):
		fast = slow = head
		trail = None
		while fast and fast.next:
			slow = slow.next
			fast = fast.next.next
			trail = trail.next if trail else None
			k -= 1
			if k == 0:
				# start `trail` when `slow` is `k` nodes away
				trail = head

		return trail.val if trail else -1


if __name__ == '__main__':
	s = Solution()
	assert s.kth_node_from_mid(None, 3) == -1
	assert s.kth_node_from_mid(ListNode.fromList([1,2]), 1) == 1
	assert s.kth_node_from_mid(ListNode.fromList([3,4,7,5,6,16,15,61,16]), 3) == 4
	assert s.kth_node_from_mid(ListNode.fromList([1,14,6,16,4,10]), 2) == 14
	assert s.kth_node_from_mid(ListNode.fromList([1,14,6,16,4,10]), 7) == -1

	
