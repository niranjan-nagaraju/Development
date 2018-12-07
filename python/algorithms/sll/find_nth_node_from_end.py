'''
Find nth node from the end of an SLL (length of the SLL is not known before hand)

Solution:
	Start a pointer, trav, and count till it reaches n nodes,
	Then start another pointer from the SLL's head, trail, 
	We would have arrived at nth node from the end (at 'trail'), when 'trav' falls off the SLL's tail end
'''


from data_structures.sll.sll import SLL
from data_structures.sll.sll import UnderFlowError

def find_last_nth(sll, n):
	trav = sll.head
	for i in range(n):
		try:
			trav = trav.next
		except: # SLL has <n items
			raise UnderFlowError


	trail = sll.head
	while trav:
		trail = trail.next
		trav = trav.next

	return trail.value



if __name__ == "__main__":
	sll1 = SLL.fromList([1,2,3,4,5,6,7,8,9,10])
	assert(find_last_nth(sll1, 3) == 8)
	assert(find_last_nth(sll1, 1) == 10)
	assert(find_last_nth(sll1, 10) == 1)
	try:
		assert(find_last_nth(sll1, 11) == None)
	except UnderFlowError:
		print "Underflow error as expected!"

