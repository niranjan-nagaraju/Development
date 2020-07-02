'''
https://www.interviewbit.com/problems/invert-the-binary-tree/

Invert the Binary Tree

Given a binary tree, invert the binary tree and return it.

Example :
Given binary tree
     1
   /   \
  2     3
 / \   / \
4   5 6   7

invert and return
     1
   /   \
  3     2
 / \   / \
7   6 5   4
'''

'''
Solution Outline:
	1. Do a recursive preorder traversal
	2. Swap left and right children if node is not a leaf node
'''
# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	def invert_binary_tree(self, root):
		def invert_helper(root):
			isLeaf = (lambda node: node.left is None and node.right is None)

			if not root or isLeaf(root):
				return

			root.left, root.right = root.right, root.left
			invert_helper(root.left)
			invert_helper(root.right)
		
		invert_helper(root)
		return root


if __name__ == '__main__':
	s = Solution()
	'''
        1
       /  \
      2    3 
      \   /
	   5 6
	'''
	root = TreeNode(1)
	root.left = TreeNode(2)
	root.right = TreeNode(3)
	root.left.right = TreeNode(5)
	root.right.left = TreeNode(6)

	root = s.invert_binary_tree(root)
	assert (root.val == 1)
	assert (root.left.val == 3)
	assert (root.right.val == 2)
	assert (root.left.right.val == 6)
	assert (root.left.left == None)
	assert (root.right.left.val == 5)
	assert (root.right.right == None)


