from dll import DLL


# Queue using a DLL
class Queue(DLL):
	def length(self):
		return self.size

	# return the node at the front of the Queue
	def frontNode(self):
		return self.head

	def front(self):
		return self.head.value

	# Override enqueue and return DLL node (will be the tail)
	def enqueue(self, value):
		self.push_back(value)
		return self.tail


	def enqueueNode(self, node):
		self.push_back(node)
		return self.tail

	# dequeue and return the DLL node (will be the previous head)
	def dequeue(self):
		return self.pop_front_node()

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
