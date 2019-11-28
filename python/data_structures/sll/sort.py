'''
Insertion sort of a singly-linked list in-place
'''

from data_structures.sll.sll import SLL

class Sort(object):
	'''
	Insertion sort:
	  Scan left-to-right, everything to the left of current node is assumed to be sorted.
	  If current node < previous node, yank it out the chain and place() it from the front of the SLL

	  NOTE:
		  if current node was yanked, retain previous node as-is,
		  else advance previous node
		  Get current node for next iteration from previous node -> next

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
		pass
