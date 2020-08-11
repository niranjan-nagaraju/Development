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
	1. The lower-bound of B, or the inorder-successor will be the last node in the path from root
		that causes a left-detour while finding the 'key', B.
'''


# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None

	def __str__(self):
		return str(self.val)


class Solution:
	# @param root : root node of tree
	# @param B : integer
	# @return the root node in the tree
	def getSuccessor(self, root, B):
		if not root:
			return None

		successor = None
		trav = root
		# search for B's lower-bound
		# and return it as inorder-successor/next-greater number
		while trav:
			if B < trav.val:
				successor = trav
				trav = trav.left
			else:
				trav = trav.right

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
	assert s.getSuccessor(root, 5).val == 6
	assert s.getSuccessor(root, 2) == root.left.right
	assert s.getSuccessor(root, 2).val == 3
	assert s.getSuccessor(root, 7) == None
	assert s.getSuccessor(root, 0).val == 1 # 'data' doesn't have to be part of the BST

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

