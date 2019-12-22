'''
Insertion sort of a singly-linked list in-place
'''

from data_structures.sll.sll import SLL

class Sort(object):
	'''
	Insertion sort:
	  Scan left-to-right, everything to the left of current node
	  is assumed to be sorted.
	  If current node < previous node,
	  yank it out the chain and place() it from the front of the SLL

	  NOTE:
		  if current node was yanked, retain previous node as-is,
		    (because we dont yet know if new current node is < or > or =)
		  else advance previous node
		  Get current node for next iteration from previous node -> next
		  (which may or may not have advanced)

	Sample run:
	2 -> 5 -> 1 -> 4 -> 3

	curr: 5, prev: 2
	5 < 2? NO

	curr: 1, prev: 5
	1 < 5
	yank: 2 -> 5 -> 4 -> 3
	place(1): 1 -> 2 -> 5 -> 4 -> 3

	curr: 4, prev: 5
	4 < 5
	yank: 1 -> 2 -> 5 -> 3
	place(4): 1 -> 2 -> 4 -> 5 -> 3

	curr: 3, prev: 5
	3 < 5
	yank: 1 -> 2 -> 4 -> 5
	place(3): 1 -> 2 -> 3 -> 4 -> 5
	'''
	@staticmethod
	def insertion_sort(sll):
		# Yank 'curr' node out of the SLL
		# 'prev' is the node before 'curr'
		# NOTE: both 'prev' and 'curr' are assumed to be non-empty (not None)
		def yank(prev, curr):
			prev.next = curr.next
			curr.next = None # sanitize 'curr' so it doesnt point to random nodes


		# Given a 'node', place it in the right position in the SLL
		# NOTE: It's guaranteed that 'node' is < than atleast one node in the SLL
		def place(node):
			# 'node' < 'head'
			# Make 'node' the new head and return
			if node.value < sll.head.value:
				node.next = sll.head
				sll.head = node
				return

			trav = sll.head
			prev = trav
			while trav.value <= node.value:
				prev = trav
				trav = trav.next

			# At this point, 'prev' is where 'node' should
			# be plaed next to
			# place 'node' between 'prev' and 'trav'
			node.next = trav
			prev.next = node


		# Begin Insertion sort #

		# Empty list - nothing to do
		if not sll.head:
			return

		# Initially only node, head, in the SLL is sorted
		prev = sll.head
		while prev.next is not None:
			curr = prev.next
			# All nodes until 'prev' is sorted
			# 'curr' < 'prev' => yank it and place it to the left
			if curr.value < prev.value:
				yank(prev, curr)
				place(curr)
			else:
				# 'curr' is already in the right place
				# advance 'prev' to next node, i,e 'curr'
				prev = curr


			
if __name__ == '__main__':
	l = [2,5,1,4,3]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()

	l = [2,4,6,1,3,5]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()

	l = [1,3,5,2,4,6]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()

	l = [1,2,3,4,5,6]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()

	l = [6,5,4,3,2,1]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()

	l = [6,6,6,1,2,3]
	s = SLL.fromList(l)
	Sort.insertion_sort(s)
	assert sorted(l) == s.toList()




