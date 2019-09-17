'''
Given a binary tree, flatten it to a linked list in-place.

For example, given the following tree:

    1
   / \
  2   5
 / \   \
3   4   6
The flattened tree should look like:

1
 \
  2
   \
    3
     \
      4
       \
        5
         \
          6
'''

# Definition for a binary tree node.
class TreeNode(object):
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None

class Solution(object):
	def flatten(self, root):
		"""
		:type root: TreeNode
		:rtype: None Do not return anything, modify root in-place instead.
		"""
		self.binTreeToSLL_pre(root)

	'''
	Helper function to merge two SLLs
	represented by their heads and tails into one SLL
	   (lh, lt): represents left subtree's head and tail nodes
	   (rh, rt): represents right subtree's head and tail nodes
	   The new merged SLL will look like
	   lh ... lt, rh, ... rt
	   (lh, rt) will be the head and tail of the merged SLL
	NOTE: Atleast one of the two SLLs are assumed to be non-empty
	'''
	@staticmethod
	def mergeSLLs(lh, lt, rh, rt):
		if not lh: # Left SLL is empty
			lh = rh
		elif not rh: # Right SLL is empty
			rt = lt
		else:
			lt.right = rh
			rh.left = None

		return (lh, rt)


	'''
	Flatten a Binary Tree into a doubly linked list in-place such that the SLL has nodes in the preorder traversal of the binary tree

	Algorithm:
		1. Recursively convert left and right subtrees to SLLs, each with their own head and tail.
		2. Merge the left and right SLLs, left followed by right
		3. Insert root to the left of the merged SLL
		   (lh, lt): represents left subtree's head and tail nodes
		   (rh, rt): represents right subtree's head and tail nodes
		   the new merged SLL will look like
		   root, lh ... lt, rh, ... rt
		   (lh, rt) will be the head and tail of the merged SLL
		3. A leaf node's SLL (head, tail) will be itself
		NOTE: Unlike inorder, the left of the root in a subtree will need to be sanitized because its left will not be changed by the merges.
		      e.g., 
			    1
			  /   \
			 2     3

			 == 2 <- 1 -> 3
			 left SLL would be [2], right SLL would be [3]
			 Merged left + right:  2 -> 3
			 Merging root,  <- 1 -> 2 -> 3
			                |  ------^  
			 Therefore root's left need to be set to None, resulting in 1 -> 2 -> 3
			 However, this needn't be done for each subtree, because each subtree's root eventually points back to _something_
			 Only the root of the whole tree would be left dangling at the end, creating a loop, and needs to be sanitized.


	Sample run
		 1
	   /   \
	  2     3
	 / \   /  \
	4   5 6   7
	expected: 1 -> 2 -> 4 -> 5 -> 3 -> 6 -> 7

	f(1):
		> f(2):
		   >> f(4):
			  leaf node => SLL: (4,4)   4
		   >> f(5)
			  leaf node => SLL: (5,5)   5
		   Merge (4,4) and (5,5) => SLL : (4,5)  4 -> 5
		   Add 2 to SLL's head => SLL: (2, 5)   2 -> 4 -> 5
		   SLL: (2,5)  2 -> 4 -> 5
		> f(3):
		   >> f(6):
			  leaf node => SLL: (6,6)   6
		   >> f(7):
			  leaf node => SLL: (7,7)   7
		   Merge (6,6) and (7,7) => SLL: (6,7) 6 -> 7
		   Add 3 to SLL's head => SLL: (3, 7)   3 -> 6 -> 7
		   SLL: (3,7)  3 -> 6 -> 7
		Merge left SLL = (2,5)  2 -> 4 -> 5, and right SLL = (3,7) 3 -> 6 -> 7
		Merged SLL: (2,7)  2 -> 4 -> 5 -> 3 -> 6 -> 7
		Add 1 to SLL's head => SLL: (1, 7)  1 -> 2 -> 4 -> 5 -> 3 -> 6 -> 7

	Sanitize 1.left to None
	Final SLL: (1,7)  1 -> 2 -> 4 -> 5 -> 3 -> 6 -> 7
	'''
	@staticmethod
	def binTreeToSLL_pre(root):
		def _binaryTreeToSLL_pre(root):
			if root == None:
				return (None, None)
			
			(lh, lt) = _binaryTreeToSLL_pre(root.left)
			(rh, rt) = _binaryTreeToSLL_pre(root.right)

			# Merge SLLs from left and right subtrees
			(lh, lt) = Solution.mergeSLLs(lh, lt, rh, rt)

			# Merge root to the previously merged (left+right) SLL with root at the front
			return Solution.mergeSLLs(root, root, lh, lt)


		# Call the helper function
		if not root:
			return
		(head, tail) =  _binaryTreeToSLL_pre(root)

		# sanitize root's left
		head.left = None


if __name__ == '__main__':
	s = Solution()
	root = TreeNode(1)
	root.left = TreeNode(2)
	root.right = TreeNode(3)
	s.flatten(root)
	l = []
	tmp = root
	while tmp is not None:
		l.append(tmp.val)
		tmp = tmp.right
	assert l == [1,2,3]


	root2 = TreeNode(1)
	root2.left = TreeNode(2)
	root2.right = TreeNode(5)
	root2.left.left = TreeNode(3)
	root2.left.right = TreeNode(4)
	root2.right.left = TreeNode(6)
	s.flatten(root2)
	l = []
	tmp = root2
	while tmp is not None:
		l.append(tmp.val)
		tmp = tmp.right
	assert l == [1,2,3,4,5,6]

