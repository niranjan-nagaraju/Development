'''
https://www.interviewbit.com/problems/postorder-traversal/

Postorder Traversal

Given a binary tree, return the postorder traversal of its nodes' values.

Example :
Given binary tree

   1
    \
     2
    /
   3
return [3,2,1].

Using recursion is not allowed.
'''

'''
Solution Outline:
    0. Use a stack to store nodes in order
    1. Start with pushing 'root' node to the stack
    2. If stack is not empty
       2.1 Pop stack top -> node, add it to postorder[]
       2.2 Push node's left, and then node's right onto the stack
       2.3 Repeat 2. until the stack is empty
    3. Reverse postorder[] and return it

Sample run:
    Tree:
        1
       /  \
      2    3 
      \   /
       5 6

    stack: [1]
    postorder: []

    stack pop: 1
      push(1.left) -> 2
      push(1.right) -> 3
      stack: [2,3]
      postorder: [1]

    stack pop: 3
      push(3.left) -> 6
      push(3.right) -> {}
      stack: [2, 6]
      postorder: [1, 3]

    stack pop: 6
      push(6.left) -> {}
      push(6.right) -> {}
      stack: [2]
      postorder: [1, 3, 6]

    stack pop: 2
      stack: []
      push(2.left) -> {}
      push(2.right) -> 5
      stack: [5]
      postorder: [1, 3, 6, 2]

    stack pop: 5
      stack: []
      push(5.left) -> {}
      push(5.right) -> {}
      stack: []
      postorder: [1, 3, 6, 2, 5]

    reverse postorder[]
      postorder: [5, 2, 6, 3, 1]
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
	def postorderTraversal(self, root):
		if not root:
			return []

		postorder = []
		stack = [root]
		while stack:
			node = stack.pop()
			stack.append(node.left) if node.left else None
			stack.append(node.right) if node.right else None
			postorder.append(node.val)

		return postorder[::-1]


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
	assert s.postorderTraversal(root) == [5,2,6,3,1]


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
	assert s.postorderTraversal(root) == [3,2,1]



