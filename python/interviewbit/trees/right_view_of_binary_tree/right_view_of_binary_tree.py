'''
https://www.interviewbit.com/problems/right-view-of-binary-tree/


Right view of Binary tree


Problem Description
Given a binary tree A of integers. Return an array of integers representing the right view of the Binary tree.
Right view of a Binary Tree: is a set of nodes visible when the tree is visited from Right side.


Problem Constraints
1 <= Number of nodes in binary tree <= 10^5
0 <= node values <= 10^9


Input Format
First and only argument is an pointer to the root of binary tree A.


Output Format
Return an integer array denoting the right view of the binary tree A.


Example Input

Input 1:

        1
      /   \
     2    3
    / \  / \
   4   5 6  7
  /
 8 

Input 2:

    1
   /  \
  2    3
   \
    4
     \
      5



Example Output
Output 1:
 [1, 3, 7, 8]

Output 2:
 [1, 3, 4, 5]

'''



'''
Solution Outline:
	1. Store `levels_done_so_far` 
	2. Start a R-L level-order traversal using a queue(node, level).
		Upon dequeue,
		2.1 print node if level > `levels_done_so_far`
		2.2 update levels_done_so_far.
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	def right_view_btree(self, root):
		if not root:
			return []

		queue = [(root, 0)]
		right_view = []
		levels_done = -1

		while queue:
			node, level = queue.pop(0)
			if level > levels_done:
				# NOTE: this can be
				# levels_done += 1
				# but is (somewhat counter-intuitively) slower.
				#  x = x+1
				#   LOAD x
				#   INC
				#   STOR x
				# vs
				# x = y
				#   LOAD y
				#   STOR x
				levels_done = level
				right_view.append(node.val)

			# Enqueue right and left children of `node`
			queue.append( (node.right, level+1) ) if node.right else None
			queue.append( (node.left, level+1) ) if node.left else None

		return right_view



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
	assert s.right_view_btree(root) == [1,3,6]


	'''
     1
      \
       2
      /
     3
	'''
	root = TreeNode(1)
	root.right = TreeNode(2)
	root.right.left = TreeNode(3)
	assert s.right_view_btree(root) == [1,2,3]

	'''
        1
      /   \
     2    3
    / \  / \
   4   5 6  7
  /
 8 
	'''
	root = TreeNode(1)
	root.left = TreeNode(2)
	root.right = TreeNode(3)
	root.left.left = TreeNode(4)
	root.left.right = TreeNode(5)
	root.right.left = TreeNode(6)
	root.right.right = TreeNode(7)
	root.left.left.left = TreeNode(8)
	assert s.right_view_btree(root) == [1,3,7,8]

	'''
    1
   /  \
  2    3
   \
    4
     \
      5
	'''
	root = TreeNode(1)
	root.left = TreeNode(2)
	root.right = TreeNode(3)
	root.left.right = TreeNode(4)
	root.left.right.right = TreeNode(5)
	assert s.right_view_btree(root) == [1,3,4,5]



