'''
https://www.interviewbit.com/problems/least-common-ancestor/

Least Common Ancestor

Find the lowest common ancestor in an unordered binary tree given two values in the tree.

 Lowest common ancestor : the lowest common ancestor (LCA) of two nodes v and w in a tree or directed acyclic graph (DAG) is the lowest (i.e. deepest) node that has both v and w as descendants. 
Example :

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4
For the above tree, the LCA of nodes 5 and 1 is 3.

LCA = Lowest common ancestor 
Please note that LCA for nodes 5 and 4 is 5.

You are given 2 values. Find the lowest common ancestor of the two nodes represented by val1 and val2
No guarantee that val1 and val2 exist in the tree. If one value doesn't exist in the tree then return -1.
There are no duplicate values.
You can use extra memory, helper functions, and can modify the node struct but, you can't add a parent pointer.
'''



'''
Solution Outline:
	1. Recursively look for either values in each subtree.
		1.1 Each subtree gets a match-result pair from its left and right subtrees.
		1.2 The subtree then reconciles the matches to determine lca, or propagates the partial match
			upwards if both values are not fully matched.
			1.2.1 If either the left subtree / right subtrees return a valid pair, lca is already determined
				  propagate the valid-pair to the upper-levels.
			1.2.2 If both subtrees return a partial match each, Mark current subtree's root as LCA
					and return itself as the match to upper levels
			1.2.3 If only one of the subtrees return a partial match,
					check if subtree's root matches one of the values, if it does, return itself as lca
					Otherwise, return the partial match as-is
	2. If a node matches either values, return itself to the parent node.
		1.2 If a subtree receives one of the values from its left, and another from its right,
				it returns itself as the LCA to higher nodes

Sample run:

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4

lca(6,4):
	lca(6,4,3): # search for 6,4 in sub-tree rooted at 3
	  lca(6,4,5):
		lca(6,4,6): <- return {6,_}
		lca(6,4,2):
			lca(6,4,7): <- return {}
			lca(6,4,4): <- return {4,_}
		lca(6,4,2): <- return {4,_}
	  lca(6,4,5):
		left: {6,_}
		right: {4,_}
		=> 5 is the lca
		<- return {5,5}

	lca(6,4,3):
		left: {5,5}
		return {5,5}
lca(6,4): 5


NOTE: This would fail if both items aren't part of the tree.
In which case, it's best to store paths to either nodes
and compare them.
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
	def lca(self, a, b, root):
		# find and return a path to 'x'
		# from root
		def find_path(x, root, path):
			if not root:
				return False

			if root.val == x:
				path.append(x)
				return True

			if find_path(x, root.left, path):
				# Left subtree found 'x'
				# Add self to the path and return true upwards
				path.append(root.val)
				return True
			if find_path(x, root.right, path):
				# right subtree found 'x'
				# Add self to the path and return true upwards
				path.append(root.val)
				return True
		
			# Couldn't find 'x' in current subtree
			return False


		paths_a = []
		find_path(a, root, paths_a)
		paths_b = []
		find_path(b, root, paths_b)

		if not paths_a or not paths_b:
			return -1

		i = 1
		lca_val = -1
		try:
			while paths_a[-i] == paths_b[-i]:
				lca_val = paths_a[-i]
				i += 1
		except IndexError:
			# One of the paths ran out
			# return last stored lca
			pass

		return lca_val



if __name__ == '__main__':
	s = Solution()
	'''

        _______3______
       /              \
    ___5__          ___1__
   /      \        /      \
   6      _2_     0        8
         /   \
         7    4
	'''

	t = TreeNode(3)
	t.left = TreeNode(5)
	t.right = TreeNode(1)

	t.left.left = TreeNode(6)
	t.left.right = TreeNode(2)
	t.right.left = TreeNode(0)
	t.right.right = TreeNode(8)

	t.left.right.left = TreeNode(7)
	t.left.right.right = TreeNode(4)

	assert s.lca(6,4,t) == 5
	assert s.lca(4,6,t) == 5
	assert s.lca(4,7,t) == 2
	assert s.lca(1,0,t) == 1
	assert s.lca(9,0,t) == -1

