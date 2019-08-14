# A Binary Search Tree implementation

import sys
sys.path.append("../binary_tree/")
from data_structures.trees.binary_tree.node import *
from data_structures.trees.binary_tree.binary_tree import *

# BST inherits Binary Tree features
#   traversal functions - pre/post/in order, level order
#   views - top/front/left/bottom views
#
# Adds its own insert and search functions using the binary search tree properties
class BST(BinaryTree):
	# A helper insert function that tries to recursively find a 
	# position for the specfied node in the BST to insert into the subtree
	# NOTE: The BST should have atleast one node in it when the helper is called.
	@staticmethod
	def _insert(root, node):
		if node.value < root.value:
			if not root.left:
				root.left = node
			else:
				BST._insert(root.left, node)
		else: # node >= root
			if not root.right:
				root.right = node
			else:
				BST._insert(root.right, node)


	# Insert a node into a BST
	def insertNode(self, node):
		# The BST is empty, Initialize tree's root with this node and return
		if not self.root:
			self.root = node
			return

		self._insert(self.root, node)


	# Insert into a BST
	# TODO: accept a comparator callback for complex values
	def insert(self, value):
		node = Node(value)
		self.size += 1
		
		self.insertNode(node)




	# Lowest common ancestor
	# In a BST, LCA is the node n of two nodes n1, and n2
	# which has the value
	# n1 < n < n2
	def lca(self, n1, n2):
		if not isinstance(n1, Node):
			n1 = Node(n1)

		if not isinstance(n2, Node):
			n2 = Node(n2)
		
		# recursively find lca
		# by finding n s.t n1<n<n2
		def lca_helper(n):
			if not n:
				return None

			# LCA is to the left of current node 'n'
			if n.value > n1.value and n.value > n2.value:
				return lca_helper(n.left)

			# LCA is to the right of current node 'n'
			if n.value < n1.value and n.value < n2.value:
				return lca_helper(n.right)

			# Found the lowest common ancestor node
			return n


		# Call LCA helper function
		return lca_helper(self.root)

			

