
class Node:
	def __init__(self, value):
		self.value = value
		self.next = None

	def __str__(self):
		return str(self.value)


class SLL:
	def __init__(self):
		self.head = None
		self.tail = None
		self.size = 0

	# Insert at front
	def insert(self, value):
		node = Node(value)
		self.size += 1

		node.next = self.head
		self.head = node

		if (not self.tail):
			self.tail = node

	# Append to the rear
	def append(self, value):
		node = Node(value)

		self.size += 1

		# No elements in the list
		if not self.tail:
			self.tail = node
			self.head = node
			return

		self.tail.next = node
		self.tail = node


	def __str__(self):
		sll_str = '[' + str(self.size) + ']: '
		trav = self.head
		while (trav):
			sll_str += str(trav) + ' '
			trav = trav.next

		return sll_str


	# Reverse an SLL(iterative version)
	def reverse(self):
		if not self.head:
			return None

		first = self.head
		second = first.next
		while second:
			third = second.next

			print first, second, third

			# Make second->first link
			second.next = first

			first = second
			second = third

		# Mark erstwhile  head.next to None, so the SLL chain ends
		self.head.next = None 

		# When all's done, 'first' is pointing to the 'tail'
		# of the SLL
		# Since we have now reversed, point 'head' to 'first',
		# but before that, Update 'tail' to erstwhile 'head'
		self.tail = self.head
		self.head = first


sll = SLL()
sll.append(1)
sll.append(2)
sll.append(3)
sll.append(4)

print sll
print sll.head, sll.tail

sll.reverse()
print sll
print sll.head, sll.tail
