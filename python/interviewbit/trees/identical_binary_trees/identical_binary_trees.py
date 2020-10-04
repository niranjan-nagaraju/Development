'''
https://www.interviewbit.com/problems/identical-binary-trees/

Identical Binary Trees

Given two binary trees, write a function to check if they are equal or not.

Two binary trees are considered equal if they are structurally identical and the nodes have the same value.

Return 0 / 1 ( 0 for false, 1 for true ) for this problem

Example :
Input : 

   1       1
  / \     / \
 2   3   2   3

Output : 
  1 or True
'''



'''
Solution Outline:
	1. Recursively check for left subtrees and right subtrees equality.
	2. If left and right subtrees both match, then check if the roots of the two subtrees match as well.
	3. Base case is leaf nodes and empty nodes, which can check trivially for equality.


Sample run:
   t1      t2

   1       1
  / \     / \
 2   3   2   3

 eq(t1, t2): eq(1, 1)
	=> Match root nodes , eq(t1.root, t2.root) = T
	=> eq(1.left, 1.left): T
		=> eq(2, 2) => T
	=> eq(t1.right, t2.right): T
		=> eq(3, 3) => T
	=> L & R subtrees match, eq(t1.root, t2.root) = T
	=> return True
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
	def areIdentical(self, A, B):
		def compareNodes(node1, node2):
			if node1 is None:
				return node2 is None
			if node2 is None:
				return node1 is None

			return node1.val == node2.val

		# compare roots, and if they match,
		# compare their left and right subtrees but only if the roots themselves weren't empty
		return compareNodes(A, B) and\
				(self.areIdentical(A.left, B.left) and\
				self.areIdentical(A.right, B.right) if A else True)



if __name__ == '__main__':
	s = Solution()

	assert s.areIdentical(None, TreeNode(5)) == False
	assert s.areIdentical(TreeNode(4), None) == False

	t1 = TreeNode(1)
	t2 = TreeNode(1)

	t1.left = TreeNode(2)
	t2.left = TreeNode(2)

	t1.right = TreeNode(3)
	t2.right = TreeNode(3)

	assert s.areIdentical(t1, t2) == True

