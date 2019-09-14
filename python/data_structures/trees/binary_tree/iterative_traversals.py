from data_structures.trees.binary_tree.binary_tree import BinaryTree
from data_structures.trees.binary_tree.node import Node
from data_structures.sll.stack import Stack
from collections import defaultdict


class IterativeTraversals(object):
	# A default print function if no aggregator is provided
	# for traversal functions
	_default_printfn = lambda x,y : sys.stdout.write(str(y))

	'''
	Inorder traversal of a binary tree (iterative version)
	Algorithm outline:
		0. Use a stack to store root nodes until the left subtrees are all printed.
		1. Start with pushing the root node to the stack.
		2. For each subtree rooted at root,
			push root.left to the stack if root.left is not 'done' yet.
			Else, pop and visit root, and push root.right to stack
		   if node's left is empty or not 'done' yet.
		3. Repeat until stack is empty, by examining stack top, and popping as necessary

	Sample run:
                                1
                              /   \	
                             2     3
                           /  \  /  \
                          4   5 6    7

		node 1:
		Stack: [1]
		visited: []

		top == 1
		1.left == 2 is not visited,
		push 2 to stack
		stack: [2,1]

		top == 2
		2.left == 4 is not visited
		push 4 to stack
		stack: [4,2,1]

		top == 4
		4.left is empty => pop 4, visit 4, push 4.right=None
		visited: [4]
		Inorder traversal: [4]
		stack: [2,1]

		top == 2
		2.left == 4 is visited => pop(2) and visit 2, push(2.right == 5)
		stack: [5,1]
		visited: [4,2]
		Inorder traversal: [4,2]

		top -- 5
		5.left is empty => pop(5), visit 5, push(5.right = None)
		visited: [4,2,5]
		Inorder traversal: [4,2,5]
		stack: [1]

		top = 1
		1.left == 2 is visited, => pop(1) and visit 1, push(1.right=3)
		stack: [3]
		visited: [4,2,5,1]
		Inorder traversal: [4,2,5,1]

		top = 3
		3.left == 6 is not done
		push 6 onto stack
		stack: [6,3]

		top = 6
		6.left is empty => pop(6), visit 6, push(6.right=None)
		visited: [4,2,5,1,6]
		Inorder traversal: [4,2,5,1,6]
		stack: [3]

		top = 3
		3.left == 6 is visited => pop(3), visit 3, push(3.right=7)
		visited: [4,2,5,1,6,3,7]
		Inorder traversal: [4,2,5,1,6,3,7]
		stack: [7]

		top = 7
		7.left is empty => pop(7), visit 7, push(7.right=None)
		visited: [4,2,5,1,6,3,7]
		Inorder traversal: [4,2,5,1,6,3,7]
		stack: []

		In-order traversal:  [4,2,5,1,6,3,7]
	'''
	@staticmethod
	def inorder_traversal(btree, aggregate_fn=_default_printfn, **kwargs):
		if not btree or not btree.root:
			return

		s = Stack()
		s.push(btree.root)
		visited = defaultdict(bool)
		while s:
			curr = s.top()
			if curr.left is None or visited[curr.left]:
				# current node's left is done
				# pop it off the stack and
				# visit it
				s.pop()
				visited[curr] = True
				aggregate_fn(kwargs, curr.value)
				s.push(curr.right) if curr.right else None
			else:
				s.push(curr.left)

