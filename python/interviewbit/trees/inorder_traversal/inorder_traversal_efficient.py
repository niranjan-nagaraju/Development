'''
https://www.interviewbit.com/problems/inorder-traversal/

Inorder Traversal

Given a binary tree, return the inorder traversal of its nodes' values.

Example :
Given binary tree

   1
    \
     2
    /
   3
return [1,3,2].

Using recursion is not allowed.
'''

'''
Solution Outline:
    0. Use a stack to store nodes in order, 
    1. Initially push all left-side children of root onto the stack starting from root.
    2. If the stack is not empty
        2.1 pop stack top 'node'
        2.2 push node's right onto the stack
        2.3 push all left-side children of node's right onto the stack
        2.4 Repeat 2. until the stack is empty

Sample run:
    Tree:
        1
       /  \
      2    3 
      \   /
       5 6

    Stack: [1, 2]
    inorder: []

    stack pop: 2
    Stack: [1]
    2's right -> 5
    push(5)
    Stack: [1, 5]
    inorder: [2]

    stack pop: 5
    Stack: [1]
    5's right -> {}
    Stack: [1]
    inorder: [2, 5]

    stack pop: 1
    Stack: []
    1's right -> 3
    push(3)
    Stack: [3]
    push all 3's left-side children
    Stack: [3, 6]
    inorder: [2, 5, 1]

    stack pop: 6
    Stack: [3]
    6's right -> {}
    Stack: [3]
    inorder: [2, 5, 1, 6]

    stack pop: 3
    Stack: []
    3's right -> {}
    Stack: []
    inorder: [2, 5, 1, 6, 3]
'''

# Definition for a  binary tree node
class TreeNode:
	def __init__(self, x):
		self.val = x
		self.left = None
		self.right = None


class Solution:
	# @param root : root node of tree
	# @return a list of integers
	def inorderTraversal(self, root):
		if not root:
			return []

		stack = []
		# Initially push all nodes in the left-path of root
		node = root
		while node:
			stack.append(node)
			node = node.left

		inorder = []
		while stack:
			node = stack.pop()
			n = node.right
			while n:
				stack.append(n)
				n = n.left
			inorder.append(node.val)

		return inorder


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
	assert s.inorderTraversal(root) == [2,5,1,6,3]


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
	assert s.inorderTraversal(root) == [1,3,2]



