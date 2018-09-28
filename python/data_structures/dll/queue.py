from dll import DLL


# Queue using a DLL
class Queue(DLL):
	def length(self):
		return self.size

	# return the node at the front of the Queue
	def frontNode(self):
		return self.head

	# return the item at the front of the Queue
	def front(self):
		return self.head.value

	# return the last node in the Queue
	def lastNode(self):
		return self.tail

	# return the item at the end of the Queue
	def last(self):
		return self.tail.value

	# Enqueue item at the back of the queue 
	def enqueue(self, value):
		self.push_back(value)

	# Enqueue a 'DLL wrapped node' at the back of the queue 
	def enqueueNode(self, node):
		self.push_back(node)

	# Dequeue node at front of the queue and return it (will be the previous head)
	def dequeueNode(self):
		return self.pop_front_node()

	# Dequeue item at the front of the queue and return the item
	def dequeue(self):
		return self.pop_front()

	# Remove specified node from the queue (wherever it is), 
	# and enqueue it back at the end of the queue
	# NOTE: No checks ar performed to see if the if 'node' indeed is part of the queue for optimization reasons.
	def reEnqueueNode(self, node):
		# if the node is already at the 
		# back of the queue, 
		# removing it and adding back to the same place
		# is wasteful
		if self.tail == node:
			return

		tmp = self.removeNode(node)
		self.enqueueNode(node)



if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)

	print queue

	trav = queue[0]
	queue.reEnqueueNode(trav)
	print queue
