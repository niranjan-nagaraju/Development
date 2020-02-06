'''
https://leetcode.com/problems/binary-tree-zigzag-level-order-traversal/

103. Binary Tree Zigzag Level Order Traversal

Given a binary tree, return the zigzag level order traversal of its nodes' values. (ie, from left to right, then right to left for the next level and alternate between).

For example:
Given binary tree [3,9,20,null,null,15,7],
    3
   / \
  9  20
    /  \
   15   7
return its zigzag level order traversal as:
[
  [3],
  [20,9],
  [15,7]
]
'''

'''
Solution Outline:
	Use a deque for an intermediate structure, defer traversing any level until all the nodes from that level are in the deque
	Always enqueue from the rear end.
	Dequeue from front if odd (LIFO), else back (FIFO)
'''


# Definition for a binary tree node.
class TreeNode(object):
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None

class Solution(object):
	def zigzagLevelOrder(self, root):
		"""
		:type root: TreeNode
		:rtype: List[List[int]]
		"""
		# Helper function to flush deque
		def flush():
			# odd: R-L
			l = []
			if curr_level & 1:
				while deq:
					l.append(deq.pop())
			else: # even: L-R
				while deq:
					l.append(deq.pop(0))

			traversal.append(l)


		# start zig-zag traversal
		if not root:
			return

		traversal = []
		deq = []
		bfs_q = [(0, root)]
		curr_level = 0
		while bfs_q:
			curr_level, node = bfs_q.pop(0)
			deq.append(node.val)
			bfs_q.append((curr_level+1, node.left)) if node.left else None
			bfs_q.append((curr_level+1, node.right)) if node.right else None

			# We just popped the last node for current level
			# flush deque L-R if curr_level is even
			# R-L if curr_level is odd
			if bfs_q and bfs_q[0][0] > curr_level:
				flush()
			

		# At the end, we are left with nodes from the last level in the deque
		flush()

		return traversal


'''
Convenience function to make a binary tree from an array representation.

The array is assumed to be an (almost) complete binary tree containing items
ordered level-by-level, with 'None' as its value if any of the node is empty.

NOTE:
	The array can end at the last non-empty node in the tree, and needn't be a complete binary tree
	with 2^h items (h: height of the binary tree)

	e.g. [1, 2, None, 4, 5]
	  1
	/   
   2
 /   \
4     5

Algorithm outline:
	Use indices to identify the child nodes of each parent node, and insert them at the appropriate place
	in the binary tree.
	In a heap-based indexing system, all odd numbered indices are left children, all even-numbered ones are right.
'''
def fromList(array):
	# lambda helper functions to get parent, left, and right nodes indices
	# from current index
	parent = lambda i:  (i-1)/2
	left = lambda i: 2*i+1
	right = lambda i: 2*i+2

	# All odd indices are left children,
	# even indices are right children in the binary tree
	isLeftChild = lambda i: (i&1 == 1)

	# root cannot be empty
	if not array or array[0] is None:
		return None

	root = TreeNode(array[0])
	nodes_list = [root]

	for i in xrange(1, len(array)):
		n = None
		if array[i] is not None:
			n = TreeNode(array[i])

		p = nodes_list[parent(i)]
		if p is None:
			continue

		if isLeftChild(i):
			p.left = n
		else:
			p.right = n

		nodes_list.append(n)

	return root



if __name__ == '__main__':
	s = Solution()
	btree = fromList([1,2,3])
	assert s.zigzagLevelOrder(btree) == [[1], [3,2]]

	'''
    3
   / \
  9  20
    /  \
   15   7
	'''
	btree = fromList([3,9,20,None,None,15,7])
	assert s.zigzagLevelOrder(btree) == [[3], [20,9], [15,7]]

