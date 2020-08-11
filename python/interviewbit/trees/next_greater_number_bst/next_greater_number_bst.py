'''
https://www.interviewbit.com/problems/next-greater-number-bst/

Next Greater Number BST

Given a BST node, return the node which has value just greater than the given node.

Example:
Given the tree

               100
              /   \
            98    102
           /  \
         96    99
          \
           97
Given 97, you should return the node corresponding to 98 as thats the value just greater than 97 in the tree.
If there are no successor in the tree ( the value is the largest in the tree, return NULL).

Using recursion is not allowed.

Assume that the value is always present in the tree.
'''



'''
Solution Outline:
    0. Search for the 'key', B by using binary search down the BST from root.
    1. If 'n' is the node containing 'B',
        1.1 If n has a right subtree,
            then the inorder-successor/next-greater-number of B in the BST
            would be the left-most node in n's right sub-tree.
        1.2 Otherwise, n's inorder-successor would be above it, and will be a node
            in the path from root -> n, which prompts the binary search to take a detour to
            the left-side.
            The last-such node which causes a left-detour will be the inorder-successor.
    2. Record nodes that cause a left-detour while searching for the node with B,
        to avoid a second walk-down from the root, incase the node does not contain a right subtree
        and there are no parent-threads/pointers to go back up.

    For example:
        consider
         a
        /
       b
        \
         c
          \
           d
            \
             e

      The inorder-successor of e would be a.
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


	def __str__(self):
		return '({} -> {} {})'.format(self.val, self.left, self.right)

class Solution:
	# @param root : root node of tree
	# @param B : integer
	# @return the root node in the tree
	def getSuccessor(self, root, B):
		if not root:
			return None

		# find the node
		tmp = root
		successor = None
		while tmp and tmp.val != B:
			if B < tmp.val:
				# record last 'left' detour taken
				successor = tmp
				tmp = tmp.left
			else:
				tmp = tmp.right

		if not tmp:
			# Couldn't find 'B' in BST
			return None

		# 'tmp' has a right subtree
		# left-most leaf in node tmp's right path
		# will be the inorder successor
		if tmp.right:
			tmp = tmp.right
			while tmp:
				prev = tmp
				tmp = tmp.left
			return prev
		else:
			return successor



if __name__ == '__main__':
	'''
         4
       /   \
      2     6 
     / \   / \
	1   3 5   7
	'''

	root = TreeNode(4)
	root.left = TreeNode(2)
	root.left.left = TreeNode(1)
	root.left.right = TreeNode(3)

	root.right = TreeNode(6)
	root.right.left = TreeNode(5)
	root.right.right = TreeNode(7)

	s = Solution()
	assert s.getSuccessor(root, 5) == root.right
	assert s.getSuccessor(root, 2) == root.left.right
	assert s.getSuccessor(root, 7) == None
	assert s.getSuccessor(root, 0) == None
	assert s.getSuccessor(root, 3) == root

	'''
               100
              /   \
            98    102
           /  \
         96    99
          \
           97

	'''
	root = TreeNode(100)
	root.left = TreeNode(98)
	root.right = TreeNode(102)
	root.left.left = TreeNode(96)
	root.left.right = TreeNode(99)
	root.left.left.right = TreeNode(97)
	assert s.getSuccessor(root, 97).val == 98
	assert s.getSuccessor(root, 99).val == 100

