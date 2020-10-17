'''
https://www.interviewbit.com/problems/symmetric-binary-tree/

Symmetric Binary Tree

Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).

Example :

    1
   / \
  2   2
 / \ / \
3  4 4  3
The above binary tree is symmetric.
But the following is not:

    1
   / \
  2   2
   \   \
   3    3
Return 0 / 1 ( 0 for false, 1 for true ) for this problem
'''


'''
Solution Outline:
	1. Start with root, and its mirror node (say root')
		1.1 compare root.val and root'.val
		1.2 Move to root.left and root'.right
			recursively check as in step 1.
		1.3 Move to root.right and root'.left
			recursively check as in step 1.

Sample run:
    1
   / \
  2   2
 / \ / \
3  4 4  3

f(1, 1'):
	1 == 1'
	f(1.left, 1.right):
		f(2, 2'):
			2 == 2
			f(2.left, 2'.right) == f(3, 3')
				3 == 3
			f(2.right, 2'.left) == f(4, 4')
				4 == 4'

return true
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
	# @return an integer
	def is_symmetric(self, root):
		def is_symmetric_r(root, root_):
			if not root or not root_:
				# If either of root or its mirror node, root_
				# is an empty node
				# check if both are empty
				return root_ == root

			return root.val == root_.val and \
					is_symmetric_r(root.left, root_.right) and \
					is_symmetric_r(root.right, root_.left)

		return int(is_symmetric_r(root, root))



if __name__ == '__main__':
	s = Solution()

	'''
    1
   / \
  2   2
 / \ / \
3  4 4  3
	'''
	t = TreeNode(1)
	t.left = TreeNode(2)
	t.right = TreeNode(2)
	t.left.left = TreeNode(3)
	t.left.right = TreeNode(4)
	t.right.left = TreeNode(4)
	t.right.right = TreeNode(3)
	assert s.is_symmetric(t) == 1

	'''
    1
   / \
  2   2
   \   \
   3    3
   '''
	t = TreeNode(1)
	t.left = TreeNode(2)
	t.right = TreeNode(2)
	t.left.right = TreeNode(3)
	t.right.right = TreeNode(3)
	assert s.is_symmetric(t) == 0

	'''
    1
   / \
  2   2
   \  /
   3 3   
   '''
	t = TreeNode(1)
	t.left = TreeNode(2)
	t.right = TreeNode(2)
	t.left.right = TreeNode(3)
	t.right.left = TreeNode(3)
	assert s.is_symmetric(t) == 1


