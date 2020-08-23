'''
https://www.interviewbit.com/problems/level-order/

Level Order

Given a binary tree, return the level order traversal of its nodes' values. (ie, from left to right, level by level).

Example :
Given binary tree

    3
   / \
  9  20
    /  \
   15   7
return its level order traversal as:

[
  [3],
  [9,20],
  [15,7]
]
Also think about a version of the question where you are asked to do a level order traversal of the tree when depth of the tree is much greater than number of nodes on a level.
'''


# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	# @param A : root node of tree
	# @return a list of list of integers
	def levelOrder(self, A):
		if not A:
			return []

		level_order_seq = []    
		q = [(A, 0)]
		while q:
			node, level = q.pop(0)
			q.append((node.left, level+1)) if node.left else None
			q.append((node.right, level+1)) if node.right else None

			try:
				level_order_seq[level].append(node.val)
			except IndexError:
				level_order_seq.append([node.val])

		return level_order_seq



if __name__ == '__main__':
	s = Solution()
	'''
    3
   / \
  9  20
    /  \
   15   7
	'''
	root = TreeNode(3)
	root.left = TreeNode(9)
	root.right = TreeNode(20)
	root.right.left = TreeNode(15)
	root.right.right = TreeNode(7)

	assert s.levelOrder(root) == \
			[
				[3],
				[9, 20],
				[15, 7],
			]

