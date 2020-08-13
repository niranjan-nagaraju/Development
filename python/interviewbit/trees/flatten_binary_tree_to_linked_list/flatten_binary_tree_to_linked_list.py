'''
https://www.interviewbit.com/problems/flatten-binary-tree-to-linked-list/

Flatten Binary Tree to Linked List

Given a binary tree, flatten it to a linked list in-place.

Example :
Given

         1
        / \
       2   5
      / \   \
     3   4   6
The flattened tree should look like:

   1
    \
     2
      \
       3
        \
         4
          \
           5
            \
             6

Note that the left child of all nodes should be NULL.
'''



'''
Solution Outline:
    0. Start a post-order traversal
    1. Recursively flatten a node's left sub-tree, then its right sub-tree
        1.1. let l be the left sub-tree's linked-list, r be the right sub-tree's linked-list
        1.2. Set node's left to NULL
        1.3. Concatenate node -> l -> r, and return node
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	# @param A : root node of tree
	# @return the root node in the tree
	def flatten(self, A):
		# recursive flatten helper
		# Create a new chain out of
		# root -> flattened(root.left) -> flattened(root.right)
		def flatten_helper(root):
			if not root:
				return None, None

			lh, lt = flatten_helper(root.left)
			root.left = None
			rh, rt = flatten_helper(root.right)

			lh, lt = concatenate(root, root, lh, lt)
			lh, lt = concatenate(lh, lt, rh, rt)
			return lh, lt


		# concatenate two lists and return the new head and tail
		def concatenate(leftH, leftT, rightH, rightT):
			if not leftH:
				return rightH, rightT
			if not rightH:
				return leftH, leftT

			leftT.right = rightH
			return leftH, rightT

		# Call the recursive flatten helper function
		head, _ = flatten_helper(A)
		return head


if __name__ == '__main__':
	s = Solution()

	'''
         1
        / \
       2   5
      / \   \
     3   4   6
	'''

	root = TreeNode(1)
	root.left = TreeNode(2)
	root.right = TreeNode(5)
	root.left.left = TreeNode(3)
	root.left.right = TreeNode(4)
	root.right.right = TreeNode(6)

	head = s.flatten(root)

	assert head == root

	i = 1
	tmp = head
	while tmp:
		assert tmp.left == None
		assert tmp.val == i
		i += 1
		tmp = tmp.right


