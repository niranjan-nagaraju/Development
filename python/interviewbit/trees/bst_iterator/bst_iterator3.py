'''
https://www.interviewbit.com/problems/bst-iterator/

BST Iterator
Implement an iterator over a binary search tree (BST). Your iterator will be initialized with the root node of a BST.
The first call to next() will return the smallest number in BST. Calling next() again will return the next smallest number in the BST, and so on.

Note: next() and hasNext() should run in average O(1) time and uses O(h) memory, where h is the height of the tree.
Try to optimize the additional space complexity apart from the amortized time complexity. 
'''

'''
Solution Outline: O(h) memory
    0. Initially push all children in the left path of root until there aren't any more left nodes left
    1. For each next() call, pop stack top, push its right node, and all left-side nodes of the right child.
        return the popped node's value
    2. hasNext(): return true if stack has items in it, false otherwise

Sample run: 
         5
       /   \
      2     6 
     / \   / \
    1   3 5    9
         \     /
          4   8
             /
            7
    Initially push all left nodes onto stack.
    stack: [5, 2, 1] <- top

    next():
        stack.pop(): 1
        stack: [5,2]
        push all right child's left nodes of 1 -> {}
        return 1
    next():
        stack.pop(): 2
        stack: [5]
        push right child(2): 3, and all left children(3) 
        stack: [5, 3]
        return 2
    next():
        stack.pop(): 3
        stack: [5]
        right(3): 4 -> push(4), push all left children(4) -> {}
        stack: [5, 4]
        return 3
    next():
        stack.pop(): 4
        stack: [5]
        right(4): {}
        stack: [5]
        return 4
    next():
        stack.pop(): 5
        stack: []
        right(5): 6 -> push(6), push all left children(6) -> {5}
        stack: [6, 5]
        return 5
    next():
        stack.pop(): 5
        stack: [6]
        right(5): {}
        stack: [6]
        return 5
    next():
        stack.pop(): 5
        stack: [6]
        right(5): {}
        stack: [6]
        return 5
    next():
        stack.pop(): 6
        stack: []
        right(6): 9, push(9), push all left children(9) -> {8, 7}
        stack: [9, 8, 7]
        return 6
    next():
        stack.pop(): 7
        stack: [9, 8]
        right(7): {}
        stack: [9, 8]
        return 7
     next():
        stack.pop(): 8
        stack: [9]
        right(8): {}
        stack: [9]
        return 8
      next():
        stack.pop(): 9
        stack: []
        right(9): {}
        stack: []
        return 9
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
		self.stack = []
		while root:
			self.stack.append(root)
			root = root.left


	# @return a boolean, whether we have a next smallest number
	def next(self):
		if not self.stack:
			raise StopIteration

		node = self.stack.pop()
		n = node.right
		while n:
			self.stack.append(n)
			n = n.left

		return node.val


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

