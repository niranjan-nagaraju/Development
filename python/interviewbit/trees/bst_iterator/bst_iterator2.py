'''
https://www.interviewbit.com/problems/bst-iterator/

BST Iterator
Implement an iterator over a binary search tree (BST). Your iterator will be initialized with the root node of a BST.
The first call to next() will return the smallest number in BST. Calling next() again will return the next smallest number in the BST, and so on.

Note: next() and hasNext() should run in average O(1) time and uses O(h) memory, where h is the height of the tree.
Try to optimize the additional space complexity apart from the amortized time complexity. 
'''

'''
Solution Outline:
	Use iterative inorder traversal, using a stack.
	next() yields the next item from the stack, hasnext() returns if the stack is empty or not
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class BSTIterator:
	# @param root, a binary search tree's root node
	def __init__(self, root):
		self.root = root
		self.stack = [root] if root else []
		# Mark root, and 'empty' nodes as visited
		self.visited = set([root, None])


	# @return a boolean, whether we have a next smallest number
	def next(self):
		isDone = lambda n: n in self.visited
		while self.stack:
			node = self.stack[-1]
			if isDone(node.left):
				self.stack.pop()
				self.visited.add(node)
				self.stack.append(node.right) if node.right else None
				return node.val
			else:
				self.stack.append(node.left) if node.left else None

		# stack is empty -> error, always call hasNext() to check before calling next()
		raise StopIteration


	# @return an integer, the next smallest number
	def hasNext(self):
		return len(self.stack) != 0


if __name__ == '__main__':
	'''
         4
       /   \
      2     6 
     / \   / \
	1   3 5   7
	'''
	root = TreeNode(4)
	root.left = TreeNode(2)
	root.right = TreeNode(6)
	root.left.left = TreeNode(1)
	root.left.right = TreeNode(3)
	root.right.left = TreeNode(5)
	root.right.right = TreeNode(7)
	it = BSTIterator(root)

	traversal = [1,2,3,4,5,6,7]
	i = 0
	while it.hasNext():
		assert it.next() == traversal[i]
		i += 1

