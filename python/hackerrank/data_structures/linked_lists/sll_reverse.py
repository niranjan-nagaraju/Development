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

def Reverse(head):
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


if __name__ == "__main__":
	nodes = [Node(x) for x in range(1, 6)]

	head = nodes[0]
	for i in range(4):
		nodes[i].next = nodes[i+1]

	tmp = head
	while tmp:
		print tmp.data,
		tmp = tmp.next

	print

	head = Reverse(head)

	tmp = head
	while tmp:
		print tmp.data,
		tmp = tmp.next

	print

