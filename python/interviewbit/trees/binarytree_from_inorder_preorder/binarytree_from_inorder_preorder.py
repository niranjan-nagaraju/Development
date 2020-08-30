'''
https://www.interviewbit.com/problems/construct-binary-tree-from-inorder-and-preorder/

Construct Binary Tree From Inorder And Preorder

Given preorder and inorder traversal of a tree, construct the binary tree.

Note: You may assume that duplicates do not exist in the tree. 
Example :

Input :
        Preorder : [1, 2, 3]
        Inorder  : [2, 1, 3]

Return :
            1
           / \
          2   3
'''


'''
Solution Outline:
    Consider the following tree:
      1
    /   \
   2     3
  / \   / \
 4   5 6   7

Inorder:   [4, 2, 5, 1, 6, 3, 7]
Preorder: [1, 2, 4, 5, 3, 6, 7]

  1. Initially, root: Preorder[0] == 1
  2. Find 1 in Inorder, Everything to the left of 1 == left subtree, ones to the right belong to the right subtree.
  3. Recursively solve for left, right subtrees and link them to root.
    left subtree: 
        Inorder:   [4,2,5] 
        Preorder: [2,4,5]
            construct([4,2,5], [2,4,5])
                root: 2
                left: [4]
                right: [5]
                2
               / \
              4   5         
    right subtree:
        Inorder:   [6,3,7]
        Preorder: [3,6,7]
            construct([6,3,7], [3,6,7])
            root: 3
            left: [6]
            right: [7]
            3
           / \ 
          6   7  
      
     1
   /   \ 
  [2]  [3]
  [2] represents the sub-tree rooted at 2,
  [3] represents the sub-tree rooted at 3

'''


# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None

	def __str__(self):
		return str(self.val)

	def __eq__(self, other):
		return self.val == other.val


class Solution:
	# @param inorder : list of integers
	# @param preorder : list of integers
	# @return the root node in the tree
	def construct_btree_from_in_pre(self, inorder, preorder):
		def construct_btree_r(lIn, hIn, lPre, hPre):
			if hIn < lIn or hPre < lPre:
				return None

			# find root in inorder traversal
			root_val = preorder[lPre]
			root_idx = inorder_lookup[root_val]

			num_left = root_idx - lIn
			root = TreeNode(root_val)
			root.left = construct_btree_r(lIn, root_idx-1, lPre+1, lPre+num_left)
			root.right = construct_btree_r(root_idx+1, hIn, lPre+num_left+1, hPre)
			return root

		# Create a reverse lookup for inorder traversal elements
		# to quickly identify root node's index in the inorder traversal array
		inorder_lookup = {}
		map(lambda (i,x): inorder_lookup.__setitem__(x, i), enumerate(inorder))
		return construct_btree_r(0, len(inorder)-1, 0, len(preorder)-1)


if __name__ == '__main__':
	s = Solution()
	root = s.construct_btree_from_in_pre([4, 2, 5, 1, 6, 3, 7], [1, 2, 4, 5, 3, 6, 7])

	assert root == TreeNode(1)
	assert (root.left, root.right) == (TreeNode(2), TreeNode(3))
	assert (root.left.left, root.left.right, root.right.left, root.right.right) == \
			(TreeNode(4), TreeNode(5), TreeNode(6), TreeNode(7))

	root = s.construct_btree_from_in_pre([3, 2, 4, 1, 5], [1, 2, 3, 4, 5])
	assert root == TreeNode(1)
	assert root.left == TreeNode(2)
	assert root.right == TreeNode(5)
	assert root.left.left == TreeNode(3)
	assert root.left.right == TreeNode(4)

