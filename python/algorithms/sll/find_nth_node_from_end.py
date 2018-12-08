'''
Find nth node from the end of an SLL 

Solution 1 - (length of the SLL is known / is calculated by making a pass before find):
	Count len(sll) - n nodes from head and return the node's value


Solution 2 - (length of the SLL is not known before hand):
	Start a pointer, trav, and count till it reaches n nodes,
	Then start another pointer from the SLL's head, trail, 
	We would have arrived at nth node from the end (at 'trail'), when 'trav' falls off the SLL's tail end

'''


from data_structures.sll.sll import SLL
from data_structures.sll.sll import UnderFlowError


'''
Use length of SLL to count len(sll)-n nodes
'''
def find_last_nth_size_known(sll, n):
	try:
		# sll[-0] == sll[0]
		if n == 0:
			return sll.head.value
	except:
		raise UnderFlowError

	r = len(sll) - n

	if r < 0:
		raise UnderFlowError

	trav = sll.head
	for i in range(r):
		trav = trav.next

	return trav.value


def find_last_nth(sll, n):
	trav = sll.head
	try:
		# sll[-0] == sll[0]
		if n == 0:
			return trav.value

		for i in range(n):
				trav = trav.next

		trail = sll.head
		while trav:
			trail = trail.next
			trav = trav.next

		return trail.value
	except: # SLL has <n items
		raise UnderFlowError


if __name__ == "__main__":
	sll1 = SLL.fromList([1,2,3,4,5,6,7,8,9,10])
	assert(find_last_nth(sll1, 0) == 1)
	assert(find_last_nth_size_known(sll1, 0) == 1)

	assert(find_last_nth(sll1, 3) == 8)
	assert(find_last_nth_size_known(sll1, 3) == 8)
	assert(find_last_nth(sll1, 1) == 10)
	assert(find_last_nth_size_known(sll1, 1) == 10)
	assert(find_last_nth(sll1, 10) == 1)
	assert(find_last_nth_size_known(sll1, 10) == 1)
	try:
		assert(find_last_nth(sll1, 11) == None)
	except UnderFlowError:
		print "1: Underflow error as expected!"

	try:
		assert(find_last_nth_size_known(sll1, 11) == None)
	except UnderFlowError:
		print "2: Underflow error as expected!"
	
