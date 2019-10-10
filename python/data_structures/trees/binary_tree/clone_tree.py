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


	'''
	Iterative version of the clone tree method

	Uses iterative post-order traversal
	'''
	@staticmethod
	def clone_i(bintree):
		s = Stack()
		# contains cloned node references, a', for each node a
		# doubles as visited[] map => if cloned_nodes[a] is not empty => a is visited
		cloned_nodes = {}

		if not bintree or not bintree.root:
			return None

		# Helper function to check if a node is already 'done'/processed
		# a node is considered 'done'
		#   if its value is None
		#   or
		#   if it has already been cloned.
		isDone = lambda node, cloned_node: True if (node is None or cloned_node is not None) else False

		s.push(bintree.root)
		while s:
			node = s.top()
			left_clone = cloned_nodes.get(node.left)
			right_clone = cloned_nodes.get(node.right)
			# Both left and right subtrees are done
			# pop 'node' off the stack, create a clone, and map the clone to the original
			# Link cloned node's left and right children
			if isDone(node.left, left_clone) and isDone(node.right, right_clone):
				s.pop()
				node_ = Node(node.value)
				node_.left = left_clone
				node_.right = right_clone
				cloned_nodes[node] = node_
			else:
				# push both left and right children to the stack
				# NOTE: This actually processes reverse post-order (right-left-root)
				#       unless we push right, followed by left onto the stack
				#       but order shouldnt really matter
				s.push(node.left) if node.left else None
				s.push(node.right) if node.right else None

		return BinaryTree(root=cloned_nodes[bintree.root])

