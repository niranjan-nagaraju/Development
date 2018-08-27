# A Binary Search Tree

from node import *
import sys

class BST:
	def __init__(self, root=None, size=0):
		self.root = root
		self.size = size

	# preorder traversal (R,l,r, l:left, r:right, R:Root)
	def preorder_traversal(self, root, aggregate_fn=None, **kwargs):
		if not root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		aggregate_fn(kwargs, root)

		self.preorder_traversal(root.left, aggregate_fn, **kwargs) 
		self.preorder_traversal(root.right, aggregate_fn, **kwargs)


	# post-order traversal (l,r,R, l:left, r:right, R:Root)
	def postorder_traversal(self, root, aggregate_fn=None, **kwargs):
		if not root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		self.postorder_traversal(root.left, aggregate_fn, **kwargs) 
		self.postorder_traversal(root.right, aggregate_fn, **kwargs)
		aggregate_fn(kwargs, root)


	# In-order traversal (l,r,R, l:left, r:right, R:Root)
	def inorder_traversal(self, root, aggregate_fn=None, **kwargs):
		if not root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		self.inorder_traversal(root.left, aggregate_fn, **kwargs) 
		aggregate_fn(kwargs, root)
		self.inorder_traversal(root.right, aggregate_fn, **kwargs)


def test_traversals():
	# prefix equation tree : "+a*bc"
	root = Node("+")
	lnode = Node("a")
	rnode = Node("*")
	root.setChildren(lnode, rnode)

	rlnode = Node("b")
	rrnode = Node("c")
	rnode.setChildren(rlnode, rrnode)

	bst = BST(root)
	print 'Preorder: ',
	bst.preorder_traversal(root)
	print

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	bst.preorder_traversal(root, collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])

	print 'Postorder: ',
	bst.postorder_traversal(root)
	print

	l = []
	bst.postorder_traversal(root, collate_fn, lst=l)
	assert (l == ['a',  'b', 'c', '*', '+'])

	print 'Inorder: ',
	bst.inorder_traversal(root)
	print

	l = []
	bst.inorder_traversal(root, collate_fn, lst=l)
	assert (l == ['a', '+', 'b', '*', 'c'])


if __name__ == "__main__":
	test_traversals()

