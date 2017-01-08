from dll import *

# Queue using a DLL
class Queue:
	def __init__(self):
		self.dll = DLL()

	# enqueue and return DLL node (will be the tail)
	def enqueue(self, value):
		self.dll.append(value)
		return self.dll.tail

	# dequeue and return the DLL node (will be the previous head)
	def dequeue(self):
		return self.dll.removeFront()

	def removeNode(self, node):
		return self.dll.removeNode(node)

	def __str__(self):
		return self.dll.__str__()




if __name__ == "__main__":
	queue = Queue()

	for i in range(1, 6):
		queue.enqueue(i)

	print queue
	print 'Removing @2', queue.dll.removeNode(queue.dll[2])
	print queue

	for i in range(1, 5):
		print queue.dequeue(),
		print queue


		
