'''
http://interviewbit.com/problems/vertical-order-traversal-of-binary-tree/

Vertical Order traversal of Binary Tree

Problem Description
Given a binary tree A consisting of N nodes, return a 2-D array denoting the vertical order traversal of A.
Go through the example and image for more details.

NOTE:
If 2 or more Tree Nodes shares the same vertical level then the one with earlier occurence in the pre-order traversal of tree comes first in the output.
Row 1 of the output array will be the nodes on leftmost vertical line similarly last row of the output array will be the nodes on the rightmost vertical line.

Problem Constraints
0 <= N <= 104

Input Format
First and only argument is an pointer to root of the binary tree A.

Output Format
Return a 2D array denoting the vertical order traversal of A.

Example Input
Input 1:
      6
    /   \
   3     7
  / \     \
 2   5     9
Input 2:
           1
         /   \
        2     3
       / \
      4   5


Example Output
Output 1:
 [
    [2],
    [3],
    [6, 5],
    [7],
    [9]
 ]
Output 2:
 [
    [4],
    [2],
    [1, 5],
    [3]
 ]
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	def vertical_order_traversal(self, root):
		if not root:
			return []

		from collections import defaultdict
		bfs_q = [(0,root)]
		widths = defaultdict(lambda: [])

		lwidth = 0
		rwidth = 0

		while bfs_q:
			width,node = bfs_q.pop(0)

			if width > rwidth:
				rwidth = width
			elif width < lwidth:
				lwidth = width

			widths[width].append(node.val)
			bfs_q.append((width-1, node.left)) if node.left else None
			bfs_q.append((width+1, node.right)) if node.right else None
	
		return [widths[x] for x in xrange(lwidth, rwidth+1)]


if __name__ == '__main__':
	s = Solution()
	'''
      6
    /   \
   3     7
  / \     \
 2   5     9
	'''

	t = TreeNode(6)
	t.left = TreeNode(3)
	t.right = TreeNode(7)
	t.left.left = TreeNode(2)
	t.left.right = TreeNode(5)
	t.right.right = TreeNode(9)

	assert s.vertical_order_traversal(t) ==  [
												[2],
												[3],
												[6, 5],
												[7],
												[9]
											 ]

	'''
           1
         /   \
        2     3
       / \
      4   5
	'''
	t = TreeNode(1)
	t.left = TreeNode(2)
	t.right = TreeNode(3)
	t.left.left = TreeNode(4)
	t.left.right = TreeNode(5)
	assert s.vertical_order_traversal(t) ==  [
												[4],
												[2],
												[1, 5],
												[3]
											 ]


	'''
          1
        /    \ 
       2      3
      / \   /   \
     4   5  6   7
               /  \ 
              8   9 
	'''

	root = TreeNode(1) 
	root.left = TreeNode(2) 
	root.right = TreeNode(3) 
	root.left.left = TreeNode(4) 
	root.left.right = TreeNode(5) 
	root.right.left = TreeNode(6) 
	root.right.right = TreeNode(7) 
	root.right.left.right = TreeNode(8) 
	root.right.right.right = TreeNode(9) 
	assert s.vertical_order_traversal(root) == [
													[4],
													[2],
													[1, 5, 6],
													[3, 8],
													[7],
													[9]
												]

