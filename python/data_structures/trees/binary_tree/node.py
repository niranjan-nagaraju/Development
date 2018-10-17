# A Binary-tree node
class Node:
	def __init__(self, value=None, left=None, right=None):
		self.value = value
		self.left = left
		self.right = right

	def setChildren(self, left=None, right=None):
		self.left = left
		self.right = right

	def isLeaf(self):
		return (self.left is None and self.right is None)

	def __str__(self):
		return str(self.value)

	# return lisp-cons style node value as 'value: (l, r)'
	def __repr__(self):
		return "%s: (%s, %s)" %(self, self.left, self.right)


if __name__ == "__main__":
	node = Node(1)
	print "%r" %(node)
	print "%s" %(node)

	lnode = Node(2)
	rnode = Node(3)
	node.setChildren(lnode, rnode)
	assert(node.left == lnode)
	assert(node.right == rnode)
	print "%r" %(node)
	print "%s %s %s" %(node, node.left, node.right)

