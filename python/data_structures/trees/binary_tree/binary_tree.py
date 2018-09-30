# A Binary Tree

import sys

sys.path.append("../../../data_structures/")
from sll.queue import Queue # import just the Queue, as sll.Node will conflict with binary_tree.node
from node import Node

class BinaryTree:
	def __init__(self, root=None, size=0):
		self.root = root
		self.size = size


	# Calculate height of the binary tree
	def height(self):
		# helper function to calculate height of a subtree rooted at 'root'
		def _height(root):
			if (not root) or root.isLeaf():
				return 0

			return (1 + max(_height(root.left), _height(root.right)))

		# call helper function to calculate height of the entire tree
		return _height(self.root)


	# Calculate width/diameter of the binary tree
	def width(self):
		pass



	# preorder traversal: (R,l,r, l:left, r:right, R:Root)
	# Traverse binary tree nodes in pre-order and return their values
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def preorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in preorder
		def _preorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			aggregate_fn(kwargs, root.value)
			_preorder_traversal(root.left, aggregate_fn, **kwargs) 
			_preorder_traversal(root.right, aggregate_fn, **kwargs)

		# Call the helper function to do the actual traversal
		_preorder_traversal(self.root, aggregate_fn, **kwargs)



	# post-order traversal (l,r,R, l:left, r:right, R:Root)
	# Traverse binary tree nodes in post-order and return their values
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def postorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in postorder
		def _postorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			_postorder_traversal(root.left, aggregate_fn, **kwargs) 
			_postorder_traversal(root.right, aggregate_fn, **kwargs)
			aggregate_fn(kwargs, root.value)

		# Call the helper function to do the actual traversal
		_postorder_traversal(self.root, aggregate_fn, **kwargs)



	# In-order traversal (l,r,R, l:left, r:right, R:Root)
	# Traverse binary tree nodes in in-order and return their values
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def inorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		# helper function to traverse a Binary subtree rooted at 'root', in In-order
		def _inorder_traversal(root, aggregate_fn, **kwargs):
			if not root:
				return

			_inorder_traversal(root.left, aggregate_fn, **kwargs) 
			aggregate_fn(kwargs, root.value)
			_inorder_traversal(root.right, aggregate_fn, **kwargs)

		# Call the helper function to do the actual traversal
		_inorder_traversal(self.root, aggregate_fn, **kwargs)



	# Level-order traversal
	# Traverse binary tree nodes level by level, and return their values
	# if no aggregate function callback is specified,
	# just print the current node's contents
	def levelorder_traversal(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		if not self.root:
			return

		q = Queue()
		q.enqueue(self.root)
		while q.length() != 0:
			tmp = q.dequeue()
			aggregate_fn(kwargs, tmp.value)

			q.enqueue(tmp.left)  if tmp.left else None
			q.enqueue(tmp.right) if tmp.right else None



	# Left-view of a binary tree
	# Return tree items that would be seen from the left side of the binary tree
	def left_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		if not self.root:
			return

		q = Queue()
		levels_done = None
		q.enqueue((0, self.root))
		while q.length() != 0:
			curr_level, node = q.dequeue()

			# Nothing has been printed in this level so far
			# NOTE: (None < 0) in python
			if curr_level > levels_done:
				aggregate_fn(kwargs, node.value)
				levels_done = curr_level

			q.enqueue((curr_level+1, node.left))  if node.left else None
			q.enqueue((curr_level+1, node.right)) if node.right else None



	# Right-view of a binary tree
	# Return tree items that would be seen from the right side of the binary tree
	def right_view(self, aggregate_fn=lambda x,y : sys.stdout.write(str(y)), **kwargs):
		if not self.root:
			return

		q = Queue()
		q.enqueue((0, self.root))
		while q.length() != 0:
			curr_level, node = q.dequeue()

			q.enqueue((curr_level+1, node.left))  if node.left else None
			q.enqueue((curr_level+1, node.right)) if node.right else None

			# peek next node in the queue to see if this is the last node in the current level
			# if yes, print it
			(level,next_entry) = q.front() if q.front() else (None, None)

			# Queue is either empty, in which case this is the rightmost node in the last level
			# *OR*, next entry in the queue is for the level after this one, so this is the rightmost in the current level
			# In both cases, current node would be visible in a right-view.
			if (not next_entry) or (level > curr_level):
				aggregate_fn(kwargs, node.value)



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

	def lowest_common_ancestor(self, node1, node2):
		pass


def TC1():
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

	assert(btree.height() == 2)

	print 'Preorder: ',
	btree.preorder_traversal()
	print

	l = []
	# define a lambda function that collates individual node values into a list
	#collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data)
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


	print 'Left View: ',
	btree.left_view()
	print

	l = []
	btree.left_view(collate_fn, lst=l)
	assert (l == ['+', 'a',  'b'])

	print 'Right View: ',
	btree.right_view()
	print

	l = []
	btree.right_view(collate_fn, lst=l)
	assert (l == ['+', '*',  'c'])

	print 'Testcase TC1 passed!'



if __name__ == "__main__":
	TC1()

