'''
https://www.interviewbit.com/problems/merge-two-sorted-lists/


Merge Two Sorted Lists

Merge two sorted linked lists and return it as a new list.
The new list should be made by splicing together the nodes of the first two lists, and should also be sorted.

For example, given following linked lists :

  5 -> 8 -> 20 
  4 -> 11 -> 15
The merged list should be :

4 -> 5 -> 8 -> 11 -> 15 -> 20
'''


'''
Solution Outline:
	1. Initialize a new linked list chain with a dummy head
		Maintain a tail so appends are efficient.
	2. Pick and yank A's head if A's head <= B's head, otherwise pick and yank B's head
	3. Append yanked node from either A or B into the new chain.
	4. When either of A or B is exhausted, Append the remaining chain to the end of the new chain.
	5. Return new chain after skipping dummy head node as the merged chain.
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
    # @param A : head node of linked list
    # @param B : head node of linked list
    # @return the head node in the linked list
    def mergeTwoLists(self, A, B):
        l = ListNode(None) #dummy head
        l_tail = l
        while A and B:
            if A.val <= B.val:
                tmp = A
                A = A.next
            else:
                tmp = B
                B = B.next
            tmp.next = None
            l_tail.next = tmp
            l_tail = tmp
            
        
        if A:
            l_tail.next = A
        elif B:
            l_tail.next = B
            
        return l.next
                

if __name__ == '__main__':
	s = Solution()
	assert s.mergeTwoLists(
						ListNode.fromList([1,3,5,7,9]),
						ListNode.fromList([2,4,6,7,8,9])).toList() == [1,2,3,4,5,6,7,7,8,9,9]
	assert s.mergeTwoLists(
						None,
						ListNode.fromList([1,9])).toList() == [1,9]
	assert s.mergeTwoLists(
						ListNode.fromList([1,9]),
						None).toList() == [1,9]
