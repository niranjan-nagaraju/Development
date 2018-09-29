# A Binary Tree

import sys

sys.path.append("../../../data_structures/")
from sll.queue import Queue # import just the Queue, as sll.Node will conflict with binary_tree.node
from node import Node

class BinaryTree:
	def __init__(self, root=None, size=0):
		self.root = root
		self.size = size


	# preorder traversal: (R,l,r, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def preorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in preorder
		def _preorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			aggregate_fn(kwargs, root)
			_preorder_traversal(root.left, aggregate_fn, **kwargs) 
			_preorder_traversal(root.right, aggregate_fn, **kwargs)

		# Call the helper function to do the actual traversal
		_preorder_traversal(self.root, aggregate_fn, **kwargs)



	# post-order traversal (l,r,R, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def postorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in postorder
		def _postorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			_postorder_traversal(root.left, aggregate_fn, **kwargs) 
			_postorder_traversal(root.right, aggregate_fn, **kwargs)
			aggregate_fn(kwargs, root)

		# Call the helper function to do the actual traversal
		_postorder_traversal(self.root, aggregate_fn, **kwargs)





	# In-order traversal (l,r,R, l:left, r:right, R:Root)
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def inorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in In-order
		def _inorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			_inorder_traversal(root.left, aggregate_fn, **kwargs) 
			aggregate_fn(kwargs, root)
			_inorder_traversal(root.right, aggregate_fn, **kwargs)

		# Call the helper function to do the actual traversal
		_inorder_traversal(self.root, aggregate_fn, **kwargs)



	# Level-order traversal
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def levelorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		if not self.root:
			return

		q = Queue()
		q.enqueue(self.root)
		while q.length() != 0:
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


	def zigzag_levelorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		pass


	def height(self):
		# helper function to calculate height of a subtree rooted at 'root'
		def _height(root):
			if (not root) or root.isLeaf():
				return 0

			return (1 + max(_height(root.left), _height(root.right)))

		# call helper function to calculate height of the entire tree
		return _height(self.root)


	def width(self):
		pass


	def lowest_common_ancestor(self, node1, node2):
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

	assert(btree.height() == 2)


if __name__ == "__main__":
	test_traversals()

