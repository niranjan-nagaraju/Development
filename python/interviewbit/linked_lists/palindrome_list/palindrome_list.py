'''
https://www.interviewbit.com/problems/palindrome-list/

Palindrome List

Given a singly linked list, determine if its a palindrome. Return 1 or 0 denoting if its a palindrome or not, respectively.

Notes:
	Expected solution is linear in time and constant in space.

For example,
	List 1-->2-->1 is a palindrome.
	List 1-->2-->3 is not a palindrome.
'''

'''
Solution Outline:
	1. Split the linked list into two parts.
	   1.1 Use a pair of slow and fast pointers, the slow one moving 1x, the fast one moving 2x nodes at a time.
	   1.2 When the fast moving node falls off the edge, the slow pointer is at n/2th node.
	   1.3 Split the linked list into two parts
	       when n is odd,
		     1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
			 slow: 1, 2, 3, 4
			 fast: 1, 3, 5, 7, <>
		   Split the list into (l1 will have 1 node more than l2) -
		     l1: 1 -> 2 -> 3 -> 4
			 l2: 5 -> 6 -> 7
		   when n is even,
		     1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8
			 slow: 1, 2, 3, 4
			 fast: 1, 3, 5, 7, <>
		   Split the list into
		     l1: 1 -> 2 -> 3 -> 4
			 l2: 5 -> 6 -> 7 -> 8
	2. Reverse second half of the linked list l2, to l2' (l2 reversed) in-place
	3. Match l1 against l2' until l2' runs out. (size of l1 >= size of l2')
	   3.1 If all l2' nodes match against nodes in l1 in order, the linked list is a palindrome
	       Otherwise, it isn't.
	4. Reverse l2' again to get back l2 and merge l2 with l1 to restore back the original linked list

Sample run 1:
	n is odd
	L: 1 -> 2 -> 3 -> 4 -> 3 -> 2 -> 1
	n = 7

	Split into 2
	l1: 1 -> 2 -> 3 -> 4
	l2: 3 -> 2 -> 1

	l2': reverse l2
	l2': 1 -> 2 -> 3

	l2' == l1 (first 3 nodes)
	=> L is a palindrome

	reverse l2' back into l2
	l2: reverse l2'
	l2: 3 -> 2 -> 1

	L: l1 + l2
	L: 1 -> 2 -> 3 -> 4 -> 3 -> 2 -> 1


Sample run 2:
	n is even
	L: 1 -> 2 -> 3 -> 4 -> 4 -> 3 -> 2 -> 1
	n = 8

	Split into 2
	l1: 1 -> 2 -> 3 -> 4
	l2: 4 -> 3 -> 2 -> 1

	l2': reverse l2
	l2': 1 -> 2 -> 3 -> 4

	l2' == l1
	=> L is a palindrome

	l2' == l1 (first 3 nodes)
	=> L is a palindrome
	reverse l2' back into l2
	l2: reverse l2'
	l2: 4 -> 3 -> 2 -> 1

	L: l1 + l2
	L: 1 -> 2 -> 3 -> 4 -> 4 -> 3 -> 2 -> 1
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
	def is_list_palindrome(self, A):
		# Reverse a linked list, starting at 'head'
		# in-place
		# return the new head of the reversed linked list
		def reverse_list(head):
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
			return a


		# split a linked list into two 'equal' parts
		# if n is odd, the left half will have 1 node more than the right half
		# return the split-point (tail of first half, head of second half)
		# NOTE: The two split lists are expected to be detached
		def split_in_half(head):
			slow = head
			fast = head
			ftail = head
			while fast:
				ftail = slow
				slow = slow.next
				fast = fast.next.next if fast.next else None

			# detach first half, ending at ftail
			# from l2 (starting at slow)
			ftail.next = None
			return (ftail, slow)


		# Compare two lists l1, and l2
		# until l2 runs out
		# len(l1) >= len(l2)
		def cmp_lists(l1, l2):
			while l2:
				if l1.val != l2.val:
					return False
				l1 = l1.next
				l2 = l2.next

			return True


		# Linked-list is empty or has a single node
		if not A or not A.next:
			return True

		# split A into two equal halves
		l1_tail, l2 = split_in_half(A)

		# reverse l2	
		l2 = reverse_list(l2)

		# match l1 against l2'
		is_palindrome = cmp_lists(A, l2)

		# reverse l2' back into l2
		l2 = reverse_list(l2)
		
		# merge detached l1, l2 into l1+l2
		# to restore the original linked list
		l1_tail.next = l2

		return is_palindrome


if __name__ == '__main__':
	s = Solution()
	ll = ListNode.fromList([1,2,3,4,4,3,2,1])
	assert s.is_list_palindrome(ll) == True
	assert ll.toList() == [1,2,3,4,4,3,2,1]

	ll = ListNode.fromList([1,2,3,4,3,2,1])
	assert s.is_list_palindrome(ll) == True
	assert ll.toList() == [1,2,3,4,3,2,1]

	ll = ListNode.fromList([1,1])
	assert s.is_list_palindrome(ll) == True
	assert ll.toList() == [1,1]

	ll = ListNode.fromList([1,2])
	assert s.is_list_palindrome(ll) == False
	assert ll.toList() == [1,2]

	ll = ListNode.fromList([1,2,3,4,5,3,2,1])
	assert s.is_list_palindrome(ll) == False
	assert ll.toList() == [1,2,3,4,5,3,2,1]

