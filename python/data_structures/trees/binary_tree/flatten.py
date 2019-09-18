from data_structures.trees.binary_tree.binary_tree import BinaryTree

class FlattenBinaryTree(object):
	'''
	Helper function to merge two DLLs
	represented by their heads and tails into one DLL
	   (lh, lt): represents left subtree's head and tail nodes
	   (rh, rt): represents right subtree's head and tail nodes
	   The new merged DLL will look like
	   lh ... lt, rh, ... rt
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
		   (rh, rt): represents right subtree's head and tail nodes
		   the new merged DLL will look like
		   lh ... lt, root, rh, ... rt
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


				
	'''
	Flatten a Binary Tree into a doubly linked list in-place such that the DLL has nodes in the preorder traversal of the binary tree

	Algorithm:
		1. Recursively convert left and right subtrees to DLLs, each with their own head and tail.
		2. Merge the left and right DLLs, left followed by right
		3. Insert root to the left of the merged DLL
		   (lh, lt): represents left subtree's head and tail nodes
		   (rh, rt): represents right subtree's head and tail nodes
		   the new merged DLL will look like
		   root, lh ... lt, rh, ... rt
		   (root, rt) will be the head and tail of the merged DLL
		3. A leaf node's DLL (head, tail) will be itself
		NOTE: Unlike inorder, the left of the root in a subtree will need to be sanitized because its left will not be changed by the merges.
		      e.g., 
			    1
			  /   \
			 2     3

			 == 2 <- 1 -> 3
			 left DLL would be [2], right DLL would be [3]
			 Merged left + right:  2 <-> 3
			 Merging root,  <- 1 <-> 2 <-> 3
			                |  ------^  
			 Therefore root's left need to be set to None, resulting in 1 <-> 2 <-> 3
			 However, this needn't be done for each subtree, because each subtree's root eventually points back to _something_
			 Only the root of the whole tree would have a left dangling at the end, creating a loop, and needs to be sanitized.


	Sample run
		 1
	   /   \
	  2     3
	 / \   /  \
	4   5 6   7
	expected: 1 <-> 2 <-> 4 <-> 5 <-> 3 <-> 6 <-> 7

	f(1):
		> f(2):
		   >> f(4):
			  leaf node => DLL: (4,4)   4
		   >> f(5)
			  leaf node => DLL: (5,5)   5
		   Merge (4,4) and (5,5) => DLL : (4,5)  4 <-> 5
		   Add 2 to DLL's head => DLL: (2, 5)   2 <-> 4 <-> 5
		   DLL: (2,5)  2 <-> 4 <-> 5
		> f(3):
		   >> f(6):
			  leaf node => DLL: (6,6)   6
		   >> f(7):
			  leaf node => DLL: (7,7)   7
		   Merge (6,6) and (7,7) => DLL: (6,7) 6 <-> 7
		   Add 3 to DLL's head => DLL: (3, 7)   3 <-> 6 <-> 7
		   DLL: (3,7)  3 <-> 6 <-> 7
		Merge left DLL = (2,5)  2 <-> 4 <-> 5, and right DLL = (3,7) 3 <-> 6 <-> 7
		Merged DLL: (2,7)  2 <-> 4 <-> 5 <-> 3 <-> 6 <-> 7
		Add 1 to DLL's head => DLL: (1, 7)  1 <-> 2 <-> 4 <-> 5 <-> 3 <-> 6 <-> 7

	Sanitize 1.left to None
	Final DLL: (1,7)  1 <-> 2 <-> 4 <-> 5 <-> 3 <-> 6 <-> 7
	'''
	@staticmethod
	def binTreeToDLL_pre(bt):
		def _binaryTreeToDLL_pre(root):
			if root == None:
				return (None, None)
			
			(lh, lt) = _binaryTreeToDLL_pre(root.left)
			(rh, rt) = _binaryTreeToDLL_pre(root.right)

			# Merge DLLs from left and right subtrees
			(lh, lt) = FlattenBinaryTree.mergeDLLs(lh, lt, rh, rt)

			# Merge root to the previously merged (left+right) DLL with root at the front
			return FlattenBinaryTree.mergeDLLs(root, root, lh, lt)


		# Call the helper function
		if not bt:
			return (None, None)
		(head, tail) =  _binaryTreeToDLL_pre(bt.root)

		# sanitize root's left
		head.left = None
		return (head, tail)


	'''
	Flatten a Binary Tree into a doubly linked list in-place such that the DLL has nodes in the postorder traversal of the binary tree

	Algorithm:
		1. Recursively convert left and right subtrees to DLLs, each with their own head and tail.
		2. Merge the left and right DLLs, left followed by right
		3. Insert root to the right of the merged DLL
		   (lh, lt): represents left subtree's head and tail nodes
		   (rh, rt): represents right subtree's head and tail nodes
		   the new merged DLL will look like
		   lh ... lt, rh, ... rt, root
		   (lh, root) will be the head and tail of the merged DLL
		3. A leaf node's DLL (head, tail) will be itself
		NOTE: Unlike inorder, the right of the root in a subtree will need to be sanitized because its left will not be changed by the merges.
		      e.g., 
			    1
			  /   \
			 2     3

			 == 2 <- 1 -> 3
			 left DLL would be [2], right DLL would be [3]
			 Merged left + right:  2 <-> 3
			 Merging root,  2 <-> 3 <-> 1
			                      ^-----|
			 Therefore root's left need to be set to None, resulting in 2 <-> 3 <-> 1
			 However, this needn't be done for each subtree, because each subtree's root eventually points back to _something_
			 Only the root of the whole tree would have a right child dangling at the end, creating a loop, and needs to be sanitized.


	Sample run
		 1
	   /   \
	  2     3
	 / \   /  \
	4   5 6   7
	expected: 4 <-> 5 <-> 2 <-> 6 <-> 7 <-> 3 <-> 1

	f(1):
		> f(2):
		   >> f(4):
			  leaf node => DLL: (4,4)   4
		   >> f(5)
			  leaf node => DLL: (5,5)   5
		   Merge (4,4) and (5,5) => DLL : (4,5)  4 <-> 5
		   Add 2 to DLL's tail => DLL: (4, 2)   4 <-> 5 <-> 2
		   DLL: (4,2)  4 <-> 5 <-> 2
		> f(3):
		   >> f(6):
			  leaf node => DLL: (6,6)   6
		   >> f(7):
			  leaf node => DLL: (7,7)   7
		   Merge (6,6) and (7,7) => DLL: (6,7) 6 <-> 7
		   Add 3 to DLL's tail => DLL: (3, 7)   6 <-> 7 <-> 3
		   DLL: (6,3)  6 <-> 7 <-> 3
		Merge left DLL = (4,2)  4 <-> 5 <-> 2, and right DLL = (6,3) 6 <-> 7 <-> 3
		Merged DLL: (4,3)  4 <-> 5 <-> 2 <-> 6 <-> 7 <-> 3
		Add 1 to DLL's tail => DLL: (4, 1)  4 <-> 5 <-> 2 <-> 6 <-> 7 <-> 3 <-> 1

	Sanitize 1.right to None
	Final DLL: (1,7)  1 <-> 2 <-> 4 <-> 5 <-> 3 <-> 6 <-> 7
	'''
	@staticmethod
	def binTreeToDLL_post(bt):
		def _binaryTreeToDLL_post(root):
			if root == None:
				return (None, None)
			
			(lh, lt) = _binaryTreeToDLL_post(root.left)
			(rh, rt) = _binaryTreeToDLL_post(root.right)

			# Merge DLLs from left and right subtrees
			(lh, lt) = FlattenBinaryTree.mergeDLLs(lh, lt, rh, rt)

			# Merge root to the previously merged (left+right) DLL with root at the end
			return FlattenBinaryTree.mergeDLLs(lh, lt, root, root)


		# Call the helper function
		if not bt:
			return (None, None)
		(head, tail) =  _binaryTreeToDLL_post(bt.root)

		# sanitize root's right
		tail.right = None
		return (head, tail)

