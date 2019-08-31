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

	Solution outline:
		Start a BFS into the tree while assigning the nodes level by level
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
