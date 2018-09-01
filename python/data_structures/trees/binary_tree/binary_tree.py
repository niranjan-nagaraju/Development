# A Binary Tree

import sys
from node import *

class BinaryTree:
	def __init__(self, root=None, size=0):
		self.root = root
		self.size = size

	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _preorder_traversal(root, aggregate_fn=None, **kwargs):
		if not root:
			return

		aggregate_fn(kwargs, root)

		BinaryTree._preorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._preorder_traversal(root.right, aggregate_fn, **kwargs)



	# preorder traversal (R,l,r, l:left, r:right, R:Root)
	def preorder_traversal(self, aggregate_fn=None, **kwargs):
		if not self.root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		self._preorder_traversal(self.root, aggregate_fn, **kwargs)



	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _postorder_traversal(root, aggregate_fn=None, **kwargs):
		if not root:
			return

		BinaryTree._postorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._postorder_traversal(root.right, aggregate_fn, **kwargs)
		aggregate_fn(kwargs, root)


	# post-order traversal (l,r,R, l:left, r:right, R:Root)
	def postorder_traversal(self, aggregate_fn=None, **kwargs):
		if not self.root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		self._postorder_traversal(self.root, aggregate_fn, **kwargs)


	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _postorder_traversal(root, aggregate_fn=None, **kwargs):
		if not root:
			return

		BinaryTree._postorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._postorder_traversal(root.right, aggregate_fn, **kwargs)
		aggregate_fn(kwargs, root)



	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _inorder_traversal(root, aggregate_fn=None, **kwargs):
		if not root:
			return

		BinaryTree._inorder_traversal(root.left, aggregate_fn, **kwargs) 
		aggregate_fn(kwargs, root)
		BinaryTree._inorder_traversal(root.right, aggregate_fn, **kwargs)



	# In-order traversal (l,r,R, l:left, r:right, R:Root)
	def inorder_traversal(self, aggregate_fn=None, **kwargs):
		if not self.root:
			return

		# if no aggregate function callback is specified,
		# just print the current node's contents
		if aggregate_fn is None:
			aggregate_fn = lambda x,y : sys.stdout.write(str(y))

		self._inorder_traversal(self.root, aggregate_fn, **kwargs)


	# A helper insert function that tries to recursively find a 
	# position for the specfied node in the BinaryTree to insert into the subtree
	# NOTE: The BinaryTree should have atleast one node in it when the helper is called.
	@staticmethod
	def _insert(root, node):
		if node.value < root.value:
			if not root.left:
				root.left = node
			else:
				BinaryTree._insert(root.left, node)
		else: # node >= root
			if not root.right:
				root.right = node
			else:
				BinaryTree._insert(root.right, node)


	# Insert a node into a BinaryTree
	def insertNode(self, node):
		# The BinaryTree is empty, Initialize tree's root with this node and return
		if not self.root:
			self.root = node
			return

		self._insert(self.root, node)


	# Insert into a BinaryTree
	# TODO: accept a comparator callback for complex values
	def insert(self, value):
		node = Node(value)
		self.size += 1
		
		self.insertNode(node)




def test_traversals():
	# prefix equation tree : "+a*bc"
	root = Node("+")
	lnode = Node("a")
	rnode = Node("*")
	root.setChildren(lnode, rnode)

	rlnode = Node("b")
	rrnode = Node("c")
	rnode.setChildren(rlnode, rrnode)

	btree = BinaryTree(root)
	print 'Preorder: ',
	btree.preorder_traversal()
	print

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	btree.preorder_traversal(collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])

	print 'Postorder: ',
	btree.postorder_traversal()
	print

	l = []
	btree.postorder_traversal(collate_fn, lst=l)
	assert (l == ['a',  'b', 'c', '*', '+'])

	print 'Inorder: ',
	btree.inorder_traversal()
	print

	l = []
	btree.inorder_traversal(collate_fn, lst=l)
	assert (l == ['a', '+', 'b', '*', 'c'])


if __name__ == "__main__":
	test_traversals()

