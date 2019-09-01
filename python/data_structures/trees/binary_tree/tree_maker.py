from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.node import Node
from data_structures.sll.queue import Queue # import just the Queue, as sll.Node will conflict with binary_tree.node



class TreeMaker(object):
	'''
	Make a binary tree from an array representation.

	The array is assumed to be an (almost) complete binary tree containing items
	ordered level-by-level, with 'None' as its value if any of the node is empty.

	NOTE:
		The array can end at the last non-empty node in the tree, and needn't be a complete binary tree
		with 2^h items (h: height of the binary tree)

		e.g. [1, 2, None, 4, 5]
		  1
		/   
	   2
	 /   \
	4     5

	Algorithm outline:
		Use indices to identify the child nodes of each parent node, and insert them at the appropriate place
		in the binary tree.
		In a heap-based indexing system, all odd numbered indices are left children, all even-numbered ones are right.
	'''
	@staticmethod
	def fromList(array):
		# lambda helper functions to get parent, left, and right nodes indices
		# from current index
		parent = lambda i:  (i-1)/2
		left = lambda i: 2*i+1
		right = lambda i: 2*i+2

		# All odd indices are left children,
		# even indices are right children in the binary tree
		isLeftChild = lambda i: (i&1 == 1)

		# root cannot be empty
		if not array or array[0] is None:
			return None

		root = Node(array[0])
		btree = BinaryTree(root)
		nodes_list = [root]

		for i in xrange(1, len(array)):
			n = None
			if array[i] is not None:
				n = Node(array[i])

			p = nodes_list[parent(i)]
			if p is None:
				continue

			if isLeftChild(i):
				p.left = n
			else:
				p.right = n

			nodes_list.append(n)

		return btree



	'''
	Create a binary tree from an in-order and pre-order traversal.

	Algorithm:
		0. pre-order traversal goes R-l-r, in-order traversal goes l-R-r [l: left, r: right, R: root]
		1. The first item in the pre-order traversal is therefore the root of the binary tree
		2. Locate the root item in in-order traversal, everything before this item's index belongs in the left
		   subtree, everything after belongs in the right.
		3. Recursively solve for left and right subtrees, with a leaf node as a base case.

	Example:
							1
						  /   \	
						 2     3
					   /  \  /  \
					  4   5 6    7

	In-order traversal:  [4,2,5,1,6,3,7]
	Pre-order traversal: [1,2,4,5,3,6,7]

	root = (1)

	f([4,2,5,1,6,3,7], [1,2,4,5,3,6,7]):
		node: 1
		left: [4,2,5], [2,4,5]
		right: [6,3,7], [3,6,7]
		(1).left = f([4,2,5], [2,4,5])
		(1).right = f([6,3,7], [3,6,7])

	f([4,2,5], [2,4,5])					f([6,3,7], [3,6,7])
		node: 2								node: 3
		left: [4], [4]						left: [6], [6]
		right: [5], [5]						right: [7], [7]
		(2).left = f([4], [4])				(3).left = f([6], [6])
		(2).right = f([5], [5])				(3).right = f([7], [7])
	'''
	@staticmethod
	def from_traversal_in_pre(inorder, preorder):
		# some checks to see if the inputs are in order (heh!)
		if not inorder or not preorder:
			return None

		if len(inorder) != len(preorder):
			return None

		# helper function to recursively build the binary tree
		# NOTE: Assumes no duplicates FIXME
		def _from_traversal(inorder, preorder):
			# Empty traversal lists, return empty node
			if not inorder:
				return None

			# Create root node for this subtree
			root = Node(preorder[0])

			# Locate root in inorder traversal sequence
			root_idx = inorder.index(preorder[0])

			root.left = _from_traversal(inorder[:root_idx], preorder[1:root_idx+1])
			root.right = _from_traversal(inorder[root_idx+1:], preorder[root_idx+1:])

			return root

		# call the helper function
		root = _from_traversal(inorder, preorder)
		return BinaryTree(root)



