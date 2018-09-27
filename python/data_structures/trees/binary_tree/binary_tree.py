# A Binary Tree

import sys

sys.path.append("../../../data_structures/")
from sll.queue import Queue # import just the Queue, as sll.Node will conflict with binary_tree.node
from node import Node

class BinaryTree:
	def __init__(self, root=None, size=0):
		self.root = root
		self.size = size

	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _preorder_traversal(root, aggregate_fn, **kwargs):
		if not root:
			return

		aggregate_fn(kwargs, root)

		BinaryTree._preorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._preorder_traversal(root.right, aggregate_fn, **kwargs)



	# preorder traversal (R,l,r, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def preorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		self._preorder_traversal(self.root, aggregate_fn, **kwargs)



	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _postorder_traversal(root, aggregate_fn, **kwargs):
		if not root:
			return

		BinaryTree._postorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._postorder_traversal(root.right, aggregate_fn, **kwargs)
		aggregate_fn(kwargs, root)


	# post-order traversal (l,r,R, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def postorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		self._postorder_traversal(self.root, aggregate_fn, **kwargs)


	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _postorder_traversal(root, aggregate_fn, **kwargs):
		if not root:
			return

		BinaryTree._postorder_traversal(root.left, aggregate_fn, **kwargs) 
		BinaryTree._postorder_traversal(root.right, aggregate_fn, **kwargs)
		aggregate_fn(kwargs, root)



	# helper function to traverse a BinaryTree in preorder
	@staticmethod
	def _inorder_traversal(root, aggregate_fn, **kwargs):
		if not root:
			return

		BinaryTree._inorder_traversal(root.left, aggregate_fn, **kwargs) 
		aggregate_fn(kwargs, root)
		BinaryTree._inorder_traversal(root.right, aggregate_fn, **kwargs)



	# In-order traversal (l,r,R, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def inorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		self._inorder_traversal(self.root, aggregate_fn, **kwargs)


	# Level-order traversal
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def levelorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		if not self.root:
			return

		q = Queue()
		q.enqueue(self.root)
		while q.size() != 0:
			tmp = q.dequeue()
			aggregate_fn(kwargs, tmp)

			q.enqueue(tmp.left)  if tmp.left else None
			q.enqueue(tmp.right) if tmp.right else None



	# Left-view of a binary tree
	# Return the nodes that would be seen from the left side of the binary tree
	def left_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		pass


	# Right-view of a binary tree
	# Return the nodes that would be seen from the right side of the binary tree
	def right_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		pass

	# Top-view of a binary tree
	# Return the nodes that would be seen from the top of the binary tree
	# NOTE: The left-right and right-left grandchildren-nodes of a node overlap, and are masked by the grandfather node
	# e.g.
	#     a
	#    / \
    #   b   c
	#  /  \/ \
	# d   ef  g
	# Top-view of the tree rooted at a, node 'a' masks nodes LR-grandchild node 'e', and RL-grandchild node 'f'
	def top_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		pass

	# Bottom-view of a binary tree
	# Return the nodes that would be seen from the bottom side of the binary tree
	def bottom_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		pass

def test_traversals():
	# prefix equation tree : "+a*bc"
	'''
	     +
	   /   \	
      a     *
	      /   \
	     b     c
	'''		 

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

	print 'Level order: ',
	btree.levelorder_traversal()
	print

	l = []
	btree.levelorder_traversal(collate_fn, lst=l)
	assert (l == ['+', 'a', '*', 'b', 'c'])


if __name__ == "__main__":
	test_traversals()

