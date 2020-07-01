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
	1. Use a stack to store nodes in order, and a 'visited' table to check if a node's left has been processed yet.
	2. Start with pushing 'root' node to the stack
	   2.1 Peek stack top
	        If stack top's left is visited, stack top can be traversed
				Pop stack top, Add stack top's value to list,
				Push stack top's right child(if it exists) to the stack.
			If stack top's left is unvisited, we are yet to process its left child
				Push stack top's left child
		2.2 Repeat 2.1 until the stack is empty

Sample run:
	Tree:
        1
       /  \
      2    3 
      \   /
	   5 6

	Stack: [1]
	inorder: []
	visited: []

	stacktop: 1
	 1's left == 2
	 visited[2] is false
	 push(2)
	 Stack: [2,1]

	stacktop: 2
	 2's left == None
	 pop(2)
	 inorder: [2]
	 visited: [2]
	 push(2.right) -> 5
	 Stack: [5,1]

	stacktop: 5
	 5's left == None
	 pop(5)
	 inorder: [2,5]
	 visited: [2,5]
	 push(5.right) -> None
	 Stack: [1]

	stacktop: 1
	 1's left == 2
	 visited[2] is True
	 pop(1)
	 inorder: [2,5,1]
	 visited: [2,5,1]
	 push(1.right) -> 3
	 Stack: [3]

	stacktop: 3
	 3's left == 6
	 visited[6] is false
	 push(6)
	 Stack: [6,3]

	stacktop: 6
	 6's left == None
	 pop(6)
	 inorder: [2,5,1,6]
	 visited: [2,5,1,6]
	 push(6.right) -> None
	 Stack: [3]

	stacktop: 3
	 3's left == None
	 pop(3)
	 inorder: [2,5,1,6,3]
	 visited: [2,5,1,6,3]
	 push(3.right) -> None
	 Stack: []

	return [2,5,1,6,3]
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

		from collections import defaultdict
		visited = defaultdict(lambda: False)
		visited[None] = True # Mark empty nodes as 'visited' by default
		inorder = []
		stack = [root]
		while stack:
			# peek stack top
			node = stack[-1]
			if visited[node.left]:
				# node's left children are visited,
				# node can now be traversed
				stack.pop()
				inorder.append(node.val)
				visited[node] = True

				# Push node's right child to the stack if it exists
				stack.append(node.right) if node.right else None
			else:
				# node's left child hasn't been processed yet
				# Push node's left child to the stack
				stack.append(node.left) # NOTE: if node.left check is redundant here

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



