from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.node import Node
from data_structures.sll.stack import Stack

class CloneTree(object):
	'''
	Clones a binary tree.

	Recursively traverse(pre-order) the binary tree, and clone root, and then link clones of left and right subtrees from root

	eg
             a
           /   \
         b       c
       /          \
      d            g

	  clone(a) -> a'
	    a' -> left = clone(b)
		    clone(b) -> b'
			  b' -> left = clone(d)
			     clone(d) -> d'
				   d' -> left = clone(None) == None
				   d' -> right = clone(None) == None
			  b' -> right = clone(None) == None
	    a' -> right = clone(c)
		    clone(c) -> c'
			  c' -> left = clone(None) == None
			  c' -> right = clone(g)
			     clone(g) -> g'
				   g' -> left = clone(None) == None
				   g' -> right = clone(None) == None
	'''
	@staticmethod
	def clone_r(bintree):
		def _clone(root):
			if not root:
				return None

			root_ = Node(root.value)
			root_.left = _clone(root.left)
			root_.right = _clone(root.right)
			return root_

		if not bintree or not bintree.root:
			return None

		return BinaryTree(root=_clone(bintree.root))

