# A Binary Search Tree implementation

import sys
sys.path.append("../binary_tree/")
from node import *
from binary_tree import *

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




def test_traversals():
	'''
		Manually construct the below tree:
			4
		  /	  \
		1      6
		      / \
		     5   7

	'''
	root = Node(4)
	lnode = Node(1)
	rnode = Node(6)
	root.setChildren(lnode, rnode)

	rlnode = Node(5)
	rrnode = Node(7)
	rnode.setChildren(rlnode, rrnode)

	bst = BST(root)
	print 'Preorder: ',
	bst.preorder_traversal()
	print

	l = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	bst.preorder_traversal(collate_fn, lst=l)
	assert (l == [4, 1, 6, 5, 7])

	print 'Postorder: ',
	bst.postorder_traversal()
	print

	l = []
	bst.postorder_traversal(collate_fn, lst=l)
	assert (l == [1, 5, 7, 6, 4])

	print 'Inorder: ',
	bst.inorder_traversal()
	print

	l = []
	bst.inorder_traversal(collate_fn, lst=l)
	assert (l == [1, 4, 5, 6, 7])



def test_insert():
	bst = BST()
	l = [2,1,4,5,3,8]
	'''
	  Resulting tree:
	          2
			/   \
		   1	  4
		        /  \
			   3    5
			         \
					  8	
	
	   Inorder: 1 2 3 4 5 8
	'''
	for x in l:
		bst.insert(x)

	assert(bst.size == len(l))
	assert(bst.root.value == 2)

	l2 = []
	# define a lambda function that collates individual node values into a list
	collate_fn = lambda kwargs, data : kwargs['lst'].append(data.value)
	bst.inorder_traversal(collate_fn, lst=l2)

	# NOTE: sorted(list): returns a new list with sorted order without modifying the input list unlike list.sort()
	assert (l2 == sorted(l))


if __name__ == "__main__":
	test_traversals()
	test_insert()

