'''
https://www.interviewbit.com/problems/insertion-sort-list/

Insertion Sort List

Sort a linked list using insertion sort.

Insertion Sort Wiki has some details on Insertion Sort as well.

Example :
Input : 1 -> 3 -> 2
Return 1 -> 2 -> 3
'''


'''
Solution Outline:
	1. At any given time in the list [L], x, [R], L is assumed to be sorted, and contains atleast one element.
		[A single element on its own is `sorted`]
	2. Scan through [L] to find a place for 'x' s.t, a, b <- [L] and a <= x < b
		Then insert x between a and b.
		[L'] now contains [L, x] in sorted order
	3. Repeat [L'], y, [R] for the next element, y,  following x until [R] is empty. => i.e. the whole list is sorted.
	4. For a linked-list implementation, Maintain L as a separate chain, => x is the head of the unsorted chain. R.
		Initially L = {}, R = LL
		pop `x` from the front of R, and place() it in L
		place(): keeps L sorted by placing new nodes at their right place.
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
	def insertion_sort(self, A):
		# place `node` inside the sorted chain
		# ensuring the chain remains sorted
		def place(node):
			trav = sorted_chain.next
			prev = sorted_chain
			while trav and node.val >= trav.val:
				prev = trav
				trav = trav.next

			# At this point, prev <= node < trav
			# prev *might* be the dummy head node if node < all values in the sorted chain
			# trav *might* be empty if node > all values in the sorted chain
			node.next = trav
			prev.next = node

		sorted_chain = ListNode(None) # dummy head
		while A is not None:
			tmp = A
			A = A.next
			tmp.next = None # sanitize
			place(tmp)

		# return 2nd node skipping past the dummy head
		return sorted_chain.next


if __name__ == '__main__':
	s = Solution()
	ll = ListNode.fromList([5,1,3,2,4,6])
	ll = s.insertion_sort(ll)
	assert ll.toList() == [1,2,3,4,5,6]

	assert s.insertion_sort(None) == None
	assert s.insertion_sort(ListNode.fromList([1,2,3,4,5])).toList() == [1,2,3,4,5]
	assert s.insertion_sort(ListNode.fromList([5,4,3,2,1])).toList() == [1,2,3,4,5]





