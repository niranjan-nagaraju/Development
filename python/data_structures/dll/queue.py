from dll import *

# Queue using a DLL
class Queue:
	def __init__(self):
		self.dll = DLL()

	def size(self):
		return self.dll.size

	# enqueue and return DLL node (will be the tail)
	def enqueue(self, value):
		self.dll.append(value)
		return self.dll.tail

	def enqueueNode(self, node):
		self.dll.appendNode(node)
		return self.dll.tail

	# dequeue and return the DLL node (will be the previous head)
	def dequeue(self):
		return self.dll.removeFront()

	def reEnqueueNode(self, node):
		# if the node is already at the 
		# back of the queue, 
		# removing it and adding back to the same place
		# is wasteful
		if self.dll.tail == node:
			return

		tmp = self.dll.removeNode(node)
		self.enqueueNode(node)


	def __str__(self):
		return self.dll.__str__()




if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)

	print queue

	trav = queue.dll[0]
	queue.reEnqueueNode(trav)
	print queue
		
