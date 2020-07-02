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
	1. Use a stack to store nodes in order, and a 'visited' table to check if a node's left has been processed yet.
	2. Start with pushing 'root' node to the stack
	   2.1 Peek stack top
	        If stack top's left and right are visited, stack top can be traversed
				Pop stack top, Add stack top's value to list,
			If stack top's left and right are unvisited, we are yet to process its left and right children
				Push stack top's right child(if it exists) to the stack.
				Push stack top's left child(if it exists) to the stack.
				NOTE: We push right child before left child onto the stack because their order would be reversed in the stack.
		2.2 Repeat 2.1 until the stack is empty

Sample run:
	Tree:
        1
       /  \
      2    3 
      \   /
	   5 6

	Stack: [1]
	postorder: []
	visited: []

	stacktop: 1
	 1.left == 2
	 1.right == 3
	 both are unvisited
	 push(3), push(2)
	 Stack: [2,3,1]

	stacktop: 2
	 2.left == None
	 2.right == 5
	 visited[5] is false
	 push(5)
	 Stack: [5,2,3,1]

	stacktop: 5
	 5.left == None
	 5.right == None
	 pop(5)
	 postorder: [5]
	 visited: [5]
	 Stack: [2,3,1]

	stacktop: 2
	 2.left == None
	 2.right == 5
	 visited[5] is true
	 pop(2)
	 postorder: [5,2]
	 visited: [5,2]
	 Stack: [3,1]

	stacktop: 3
	 3.left == 6
	 3.right == None
	 visited[6] is false
	 push(6)
	 Stack: [6,3,1]

	stacktop: 6
	 6.left == None
	 6.right == None
	 pop(6)
	 postorder: [5,2,6]
	 visited: [5,2,6]
	 Stack: [3,1]

	stacktop: 3
	 3.left == 6
	 3.right == None
	 visited[6] is true
	 pop(3)
	 postorder: [5,2,6,3]
	 visited: [5,2,6,3]
	 Stack: [1]

	stacktop: 1
	 1.left == 2
	 1.right == 3
	 both 2 and 3 are visited
	 pop(1)
	 postorder: [5,2,6,3,1]
	 visited: [5,2,6,3,1]
	 Stack: []

	return [5,2,6,3,1]
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

		from collections import defaultdict
		visited = defaultdict(lambda: False)
		visited[None] = True # Mark empty nodes as 'visited' by default
		postorder = []
		stack = [root]
		while stack:
			# peek stack top
			node = stack[-1]
			if visited[node.left] and visited[node.right]:
				# node's left and right children have been visited
				# traverse current node
				stack.pop()
				postorder.append(node.val)
				visited[node] = True
			else:
				# Node's left or right hasn't been visited yet
				# push right node before left node, onto the stack
				# so the orders are reversed in the traversal
				# and we visit left before right
				stack.append(node.right) if node.right else None
				stack.append(node.left) if node.left else None
		return postorder


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



