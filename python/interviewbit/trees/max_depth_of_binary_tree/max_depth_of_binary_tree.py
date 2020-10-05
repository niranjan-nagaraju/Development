'''
https://www.interviewbit.com/problems/max-depth-of-binary-tree/

Max Depth of Binary Tree

Given a binary tree, find its maximum depth.

The maximum depth of a binary tree is the number of nodes along the longest path from the root node down to the farthest leaf node.

 NOTE : The path has to end on a leaf node. 
 Example :
          1
          /
         2
max depth = 2.
'''


'''
Solution Outline:
	1. Recursively calculate max-depth of a subtree
		by adding 1 to the max-depth calculated from its left and right subtrees.
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	# @param root : root node of tree
	# @return an integer
	def maxDepth(self, root):
		if not root:
			return 0

		return 1 + max(\
				self.maxDepth(root.left),\
				self.maxDepth(root.right))


if __name__ == '__main__':
	s = Solution()
	
	t1 = TreeNode(1)
	t1.left = TreeNode(2)
	t1.right = TreeNode(3)
	t1.left.left = TreeNode(4)
	assert s.maxDepth(t1) == 3

	assert s.maxDepth(TreeNode(111)) == 1


