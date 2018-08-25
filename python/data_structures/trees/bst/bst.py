# A Binary Search Tree

from node import *

class BST:
	def __init__(self, root=None):
		self.root = root

	def preorder_traversal(self, root, aggregate_fn=None, **kwargs):
		if not root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			print "%s" %(root),
		else:
			aggregate_fn(kwargs, root)

		self.preorder_traversal(root.left, aggregate_fn, **kwargs) 
		self.preorder_traversal(root.right, aggregate_fn, **kwargs)


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

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	bst.preorder_traversal(root, collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])
