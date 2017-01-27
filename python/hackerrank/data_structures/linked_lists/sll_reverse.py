"""
 https://www.hackerrank.com/challenges/reverse-a-linked-list

 Reverse a linked list
 head could be None as well for empty list
 Node is defined as
"""

class Node(object):
	def __init__(self, data=None, next_node=None):
		self.data = data
		self.next = next_node

"""
 return back the head of the linked list in the below method.
"""


# iterative version
def Reverse_1(head):
	if not head:
		return None

	first = head
	second = first.next
	while second:
		third = second.next

		# Make second->first link
		second.next = first

		first = second
		second = third

	# When all's done, 'first' is pointing to the 'tail'
	# of the SLL
	# Since we have now reversed, point 'head' to 'first',
	# But before that, set current 'head.next' to None, so the list ends
	head.next = None
	head = first

	return head


# Non-tail-recursion version
def Reverse_2(head):
	if not head:
		return None

	tmp = head

	# Reverse_2_r returns unterminated list where 'new' tail.next is not None
	head = Reverse_2_r(head)

	# Fix the tail
	tmp.next = None

	return head



# Non-tail-Recursive version
# Fix links Right->Left
# At the end, original head node (now tail) still continues to point to 
# second node (essentially forming a loop)
# Fix this at the caller
def Reverse_2_r(node):
	# Last node, Return this as head
	# this same node will be propagated
	# all the way back till the first call to this function
	if not node.next:
		return node

	head = Reverse_2_r(node.next)

	# At this point, We are atleast 2 nodes away from the end
	next = node.next
	next.next = node

	return head



# Tail-recursive reverse function
def Reverse_3_r(node, prev):
	# head: None, return immediately
	# Last node: return this as 'head'
	if not node or not node.next:
		node.next = prev
		return node

	# We are atleast 2 nodes from the end, at this point
	next = node.next

	# Link current node back to the node before it in the chain
	node.next = prev

	return Reverse_3_r(next, node)



def Reverse_3(head):
	return Reverse_3_r(head, None)


# Helper function to print SLL
def print_sll(head):
	tmp = head
	while tmp:
		print tmp.data,
		tmp = tmp.next

	print


if __name__ == "__main__":
	nodes = [Node(x) for x in range(1, 6)]

	head = nodes[0]
	for i in range(4):
		nodes[i].next = nodes[i+1]

	print 'Original list:',
	print_sll(head)

	print 'Reverse (iterative):',
	head = Reverse_1(head)
	print_sll(head)

	print 'Reverse (non-tail-recursive):',
	head = Reverse_2(head)
	print_sll(head)

	print 'Reverse (tail-recursive):',
	head = Reverse_3(head)
	print_sll(head)
