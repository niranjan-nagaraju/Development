'''
https://www.interviewbit.com/problems/preorder-traversal/

Preorder Traversal

Given a binary tree, return the preorder traversal of its nodes' values.

Example :
Given binary tree

   1
    \
     2
    /
   3
return [1,2,3].

Using recursion is not allowed.
'''

'''
Solution Outline:
	1. Use a stack to store nodes in order, and do a regular DFS traversal
	2. Start with pushing 'root' node to the stack
	   2.1 Pop stack top, and add its value to the list
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
	preorder: []

	stacktop: 1
	 pop()
	 preorder = [1]
	 push(1.right) -> 3
	 push(1.left) -> 2
	 Stack: [2,3]
	
	stacktop: 2
	 pop()
	 preorder = [1,2]
	 push(2.right) -> 5
	 push(2.left) -> None
	 Stack: [5,3]
	
	stacktop: 5
	 pop()
	 preorder = [1,2,5]
	 Stack: [3]

	stacktop: 3
	 pop()
	 preorder = [1,2,5,3]
	 push(3.right) -> None
	 push(3.left) -> 6
	 Stack: [6]

	stacktop: 6
	 pop()
	 preorder = [1,2,5,3,6]
	 Stack: []

	return [1,2,5,3,6]
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
	def preorderTraversal(self, root):
		if not root:
			return []

		preorder = []
		stack = [root]
		while stack:
			node = stack.pop()
			preorder.append(node.val)

			# push right node before left node, onto the stack
			# so the orders are reversed in the traversal
			# and we visit left before right
			stack.append(node.right) if node.right else None
			stack.append(node.left) if node.left else None
		return preorder


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
	assert s.preorderTraversal(root) == [1,2,5,3,6]


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
	assert s.preorderTraversal(root) == [1,2,3]



