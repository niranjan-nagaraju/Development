# A Binary Tree

import sys

sys.path.append("../../../data_structures/")
from sll.queue import Queue # import just the Queue, as sll.Node will conflict with binary_tree.node
from sll.sll import SLL
from node import Node

class BinaryTree:
	# A default print function if no aggregator is provided
	# for traversal functions
	_default_printfn = lambda x,y : sys.stdout.write(str(y))

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
	def preorder_traversal(self, aggregate_fn=_default_printfn, **kwargs):
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
	def postorder_traversal(self, aggregate_fn=_default_printfn, **kwargs):
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
	def inorder_traversal(self, aggregate_fn=_default_printfn, **kwargs):
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
	def levelorder_traversal(self, aggregate_fn=_default_printfn, **kwargs):
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
	def left_view(self, aggregate_fn=_default_printfn, **kwargs):
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
	def right_view(self, aggregate_fn=_default_printfn, **kwargs):
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
	# Return the node items that would be seen from the top of the binary tree
	# NOTE: The left-right and right-left grandchildren-nodes of a node overlap, and are masked by the grandfather node
	# e.g.
	#     a
	#    / \
    #   b   c
	#  /  \/ \
	# d   ef  g
	# Top-view of the tree rooted at a, node 'a' masks nodes LR-grandchild node 'e', and RL-grandchild node 'f'
	#
	# Vizualize the algorithm as an cone increasing in diameter, and printing all the nodes on the edge of the cone,
	# (assume the tree is completely full), and ignoring everything within the cone itself as they would be masked by
	# the outer nodes when viewed from the top.
	def top_view(self, aggregate_fn=_default_printfn, **kwargs):
		if not self.root:
			return

		q = Queue()
		max_left_width = 0
		max_right_width = 0
		q.enqueue((0, self.root))

		# Top-view begins with the root node
		aggregate_fn(kwargs, self.root.value)
		while q.length() != 0:
			width, node = q.dequeue()

			if width < 0:
				if width < max_left_width:
					max_left_width = width
					aggregate_fn(kwargs, node.value)
			elif width > 0:
				if width > max_right_width:
					max_right_width = width
					aggregate_fn(kwargs, node.value)

			# NOTE: width 0, is root, would already be filled in at root,
			# and in a top-view is not going to replaced

			q.enqueue((width-1, node.left))  if node.left else None
			q.enqueue((width+1, node.right)) if node.right else None


	# Top-view of a binary tree, but ordered from Left->Right
	# Return the node items that would be seen from the top of the binary tree, but after ordering them from left->right
	# NOTE: The left-right and right-left grandchildren-nodes of a node overlap, and are masked by the grandfather node
	# e.g.
	#     a
	#    / \
    #   b   c
	#  /  \/ \
	# d   ef  g
	# Top-view of the tree rooted at a, node 'a' masks nodes LR-grandchild node 'e', and RL-grandchild node 'f'
	# In the sample tree above,
	# traditional top-view (level by level) would be
	#   a b c d g
	# whereas, a L-R top-view would yield
	#  d b a c g
	def top_view_LR(self, aggregate_fn=_default_printfn, **kwargs):
		if not self.root:
			return

		# Queue to help with level-order traversal
		q = Queue()

		# Store L/R width of a node and its value if it is visible in the top-view, in a sorted list
		# ordered by the width
		# at the end of the level-order traversal the list would contain
		# [(-max_left_width, node value), ...., (0, root.value), ... (max_right_width, node value)]
		slist = SLL()

		# pairs a(w1, node_val1) vs b(w2, node_val2) are valued against each other
		# depending on w1 vs w2 (where w1 and w2 are widths, so slist is kept sorted by the nodes' L/R widths)
		pair_comparator = lambda (w1, node_val1), (w2, node_val2): cmp(w1, w2)

		max_left_width = 0
		max_right_width = 0
		q.enqueue((0, self.root))

		# Top-view begins with the root node
		slist.place((0, self.root.value))
		while q.length() != 0:
			width, node = q.dequeue()

			if width < 0:
				if width < max_left_width:
					max_left_width = width
					slist.place((width, node.value))
			elif width > 0:
				if width > max_right_width:
					max_right_width = width
					slist.place((width, node.value))

			# NOTE: width 0, is root, would already be filled in at root,
			# and in a top-view is not going to replaced

			q.enqueue((width-1, node.left))  if node.left else None
			q.enqueue((width+1, node.right)) if node.right else None


		# At the end of the level-order traversal,
		# slist is ordered by width, so
		# (-max_left_width, ..., 0, ..., max_right_width)
		# Just retrieve the SLL L-R for the top view (L-R)
		while slist.size!= 0:
			width, item = slist.pop_front()
			aggregate_fn(kwargs, item)



	# Bottom-view of a binary tree
	# Return the nodes that would be seen from the bottom side of the binary tree
	def bottom_view(self, aggregate_fn=_default_printfn, **kwargs):
		if not self.root:
			return

		q = Queue()

		# A hash table that stores a map of L/R width to a node's item
		# We traverse the tree in level-order, and keep replacing slist[width] if we find a new node with the same width
		# At the end of the traversal, every slist[width] from {-max_left_width, ...,  0, ..., max_right_width} will contain the
		# node items that would be visible from a bottom view of the binary tree
		slist = {}

		max_left_width = 0
		max_right_width = 0
		q.enqueue((0, self.root))

		# Star with placing root at width 0
		slist[0] = self.root.value
		while q.length() != 0:
			width, node = q.dequeue()

			# As we traverse level-by-level, keep replacing
			# item at L/R width
			slist[width] = node.value

			# Track max_left_width, max_right_width
			# so we don't need to sort the hash table slist,
			# Just retrieve slist[{-max_left_width, ...,  0, ..., max_right_width}]
			if width < 0:
				if width < max_left_width:
					max_left_width = width
			elif width > 0:
				if width > max_right_width:
					max_right_width = width

			q.enqueue((width-1, node.left))  if node.left else None
			q.enqueue((width+1, node.right)) if node.right else None

		# At the end of the level-order traversal,
		# Just 'aggregate' slist[{-max_left_width, ...,  0, ..., max_right_width}]
		for i in range(max_left_width, max_right_width+1):
			aggregate_fn(kwargs, slist[i])



	def zigzag_levelorder_traversal(self, aggregate_fn=_default_printfn, **kwargs):
		pass

	# Return a path, as a list of nodes, from root to the specified node in the binary tree
	def path(self, data):
		# Find path to 'node' from the subtree rooted at 'root'
		# and append each node in the path to the list
		# Steps:
		#   1. Start a post-order traversal
		#   2. Find path to node in left subtree, if not found, find in the right subtree
		#   3. If either of the left subtree/ right subtree returns True, indicating subtree contains the node,
		#        add current root node to list and return True to higher level calls
		#   4. If a subtree's root matches node, we have hit the node we were looking for, start constructing
		#	     path from here -> add node to list. return True
		def path_helper(root, node, path_nodes):
			if not root:
				return False
			
			# Found node in the subtree
			# add it to the path and return true
			# else, recursively look for it in the left/right subtree
			if root.value == node.value:
				path_nodes.insert(0, root)
				return True
			elif path_helper(root.left, node, path_nodes):
				path_nodes.insert(0, root)
				return True
			elif path_helper(root.right, node, path_nodes):
				path_nodes.insert(0, root)
				return True

				
		if not data:
			return []

		# Find path to a node with value mentioned in 'data'
		if not isinstance(data, Node):
			node = Node(data)
		else:
			node = data

		paths = []
		path_helper(self.root, node, paths)
		return paths


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

	print 'Top View: ',
	btree.top_view()
	print

	l = []
	btree.top_view(collate_fn, lst=l)
	assert (l == ['+', 'a',  '*', 'c'])

	print 'Top View L-R: ',
	btree.top_view_LR()
	print

	l = []
	btree.top_view_LR(collate_fn, lst=l)
	assert (l == ['a', '+',  '*', 'c'])


	print 'Bottom View: ',
	btree.bottom_view()
	print

	l = []
	btree.bottom_view(collate_fn, lst=l)
	assert (l == ['a', 'b',  '*', 'c'])

	print 'Testcase TC1 passed!'



def test_path():
	'''
		 1
	   /   \    
	  2     3
	 /  \  /  \
	4    5 6   7
	'''      

	root = Node(1)
	lnode = Node(2)
	rnode = Node(3)
	root.setChildren(lnode, rnode)

	lnode.setChildren(Node(4), Node(5))
	rnode.setChildren(Node(6), Node(7))
	btree = BinaryTree(root)

	assert(btree.height() == 2)

	nodes =  btree.path(7)
	assert(len(nodes) == 3)
	for n in nodes:
		print n.value

	# find path using a node reference
	nodes =  btree.path(rnode)
	for n in nodes:
		print n.value

	assert(btree.path(8) == [])


if __name__ == "__main__":
	TC1()
	test_path()
