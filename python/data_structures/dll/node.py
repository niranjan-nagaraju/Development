# DLL Node
class Node:
	def __init__(self, value=None, prev=None, next=None):
		self.value = value
		self.prev = prev
		self.next = next

	def __str__(self):
		return str(self.value)


	def __repr__(self):
		return repr(self.value)
