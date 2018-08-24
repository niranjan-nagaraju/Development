# A Binary Search Tree

from node import *

class BST:
	def __init__(self, root=None):
		self.root = root

	def preorder_traversal(self, root):
		if not root:
			return

		print "%s" %(root),
		self.preorder_traversal(root.left)
		self.preorder_traversal(root.right)



if __name__ == "__main__":
	# prefix equation tree : "+a*bc"
	root = Node("+")
	lnode = Node("a")
	rnode = Node("*")
	root.setChildren(lnode, rnode)

	rlnode = Node("b")
	rrnode = Node("c")
	rnode.setChildren(rlnode, rrnode)

	bst = BST(root)
	bst.preorder_traversal(root)

