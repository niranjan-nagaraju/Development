from data_structures.trees.binary_tree.binary_tree import BinaryTree

class FlattenBinaryTree(object):
	'''
	Helper function to merge two DLLs
	represented by their heads and tails into one DLL
	   (lh, lt): represents left subtree's head and tail nodes
	   (rh, rh): represents right subtree's head and tail nodes
	   The new merged DLL will look like
	   lh ... lt, rh, ... rh
	   (lh, rt) will be the head and tail of the merged DLL
	NOTE: Atleast one of the two DLLs are assumed to be non-empty
	'''
	@staticmethod
	def mergeDLLs(lh, lt, rh, rt):
		if not lh: # Left DLL is empty
			lh = rh
		elif not rh: # Right DLL is empty
			rt = lt
		else:
			lt.right = rh
			rh.left = lt

		return (lh, rt)


	'''
	Flatten a Binary Tree into a doubly linked list in-place such that the DLL has nodes in the inorder traversal of the binary tree

	Algorithm:
		1. Recursively convert left and right subtrees to DLLs, each with their own head and tail.
		2. Insert root in between the two DLLs merging the two DLLs into one.
		   (lh, lt): represents left subtree's head and tail nodes
		   (rh, rh): represents right subtree's head and tail nodes
		   the new merged DLL will look like
		   lh ... lt, root, rh, ... rh
		   (lh, rt) will be the head and tail of the merged DLL
		3. A leaf node's DLL (head, tail) will be itself

	Sample run
		 1
	   /   \
	  2     3
	 / \   /  \
	4   5 6   7
	expected: 4 <-> 2 <-> 5 <-> 1 <-> 6 <-> 3 <-> 7

	f(1):
	    > f(2):
	       >> f(4):
		      leaf node => DLL: (4,4)   4
		   Add 2 to DLL's tail => DLL: (4, 2)   4 <-> 2
		   >> f(5)
		      leaf node => DLL: (5,5)   5
		   Merge (4,2) and (5,5)
		   DLL: (4,5)  4 <-> 2 <-> 5
		Add 1 to DLL's tail => DLL: (4,1)  4 <-> 2 <-> 5 <-> 1
	    > f(3):
	       >> f(6):
		      leaf node => DLL: (6,6)   6
		   Add 3 to DLL's tail => DLL: (6, 3)   6 <-> 3
	       >> f(7):
		      leaf node => DLL: (7,7)   7
		   Merge (6,3) and (7,7)
		   DLL: (6,7)  6 <-> 3 <-> 7
		Merge left DLL = (4,1)  4 <-> 2 <-> 5 <-> 1, and right DLL = (6,7) 6 <-> 3 <-> 7
		Final DLL: (4,7)  4 <-> 2 <-> 5 <-> 1 <-> 6 <-> 3 <-> 7
	'''
	@staticmethod
	def binTreeToDLL_in(bt):
		def _binaryTreeToDLL_in(root):
			if root == None:
				return (None, None)
			
			(lh, lt) = _binaryTreeToDLL_in(root.left)

			# Merge current subtree's root to left subtree's DLL's tail
			(lh, lt) = FlattenBinaryTree.mergeDLLs(lh, lt, root, root)

			(rh, rt) = _binaryTreeToDLL_in(root.right)

			# Merge (left+root) and right subtrees-based DLLs
			return FlattenBinaryTree.mergeDLLs(lh, lt, rh, rt)


		# Call the helper function
		if not bt:
			return (None, None)
		return _binaryTreeToDLL_in(bt.root)
				

