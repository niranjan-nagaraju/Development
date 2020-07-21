'''
https://www.interviewbit.com/problems/sort-binary-linked-list/?ref=similar_problems

Sort Binary Linked List

Problem Description
Given a Linked List A consisting of N nodes.
The Linked List is binary i.e data values in the linked list nodes consist of only 0's and 1's.
You need to sort the linked list and return the new linked list.

NOTE:
Try to do it in constant space.


Problem Constraints
1 <= N <= 105
A.val = 0 or A.val = 1

Input Format
First and only argument is the head pointer of the linkedlist A.

Output Format
Return the head pointer of the new sorted linked list.

Example Input
Input 1:
 1 -> 0 -> 0 -> 1
Input 2:
 0 -> 0 -> 1 -> 1 -> 0

Example Output
Output 1:
 0 -> 0 -> 1 -> 1
Output 2:
 0 -> 0 -> 0 -> 1 -> 1

Example Explanation
Explanation 1:
 The sorted linked list looks like:
  0 -> 0 -> 1 -> 1
Explanation 2:
 The sorted linked list looks like:
  0 -> 0 -> 0 -> 1 -> 1
'''


'''
Solution Outline:
	1. Initialize two new chains, l0 and l1 to store the 0s and 1s respectively.
	2. Yank one node at a time from the input linked list, and append them to l0 if the node contains a 0,
		to l1 if it contains a 1
	3. Append l1 to the end of l0 and return l0 as the sorted linked list
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
    # @param A : head node of linked list
    # @return the head node in the linked list
    def sort_binary_list(self, A):
        l0 = ListNode(None) # Chain of 0s, dummy head
        l0_tail = l0
        l1 = ListNode(None) # Chain of 1s, dummy head
        l1_tail = l1
        while A:
            tmp = A
            A = A.next
            tmp.next = None
            if tmp.val == 0:
                l0_tail.next = tmp
                l0_tail = tmp
            else: # tmp.val == 1:
                l1_tail.next = tmp
                l1_tail = tmp
        
        # Append the 1s chain to the end of 0s chain        
        l0_tail.next = l1.next
        
        # return 0s chain after skipping the dummy head
        return l0.next


if __name__ == '__main__':
	s = Solution()
	assert s.sort_binary_list(ListNode.fromList([1,0,0,1])).toList() == [0,0,1,1]
	assert s.sort_binary_list(ListNode.fromList([0,0,1,1,0])).toList() == [0,0,0,1,1]
	
