
class Node:
	def __init__(self, value=None):
		self.value = value
		self.next = None

	def __str__(self):
		return "{0}".format(self.value)

	def __repr__(self):
		return "%r" %(self.value)
