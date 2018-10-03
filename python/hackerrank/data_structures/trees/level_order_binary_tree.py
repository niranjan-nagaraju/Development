'''
https://www.hackerrank.com/challenges/tree-level-order-traversal/problem
'''

class Node:
	def __init__(self, info): 
		self.info = info  
		self.left = None  
		self.right = None 
		self.level = None 

	def __str__(self):
		return str(self.info) 

class BinarySearchTree:
	def __init__(self): 
		self.root = None

	def create(self, val):  
		if self.root == None:
			self.root = Node(val)
		else:
			current = self.root

			while True:
				if val < current.info:
					if current.left:
						current = current.left
					else:
						current.left = Node(val)
						break
				elif val > current.info:
					if current.right:
						current = current.right
					else:
						current.right = Node(val)
						break
				else:
					break



"""
Node is defined as
self.left (the left child of the node)
self.right (the right child of the node)
self.info (the value of the node)
"""
def levelOrder(root):
	if not root:
		return

	q = []

	# Top-view begins with the root node
	q.append(root)
	while len(q) != 0:
		node = q.pop(0)
		print node,

		q.append(node.left)  if node.left else None
		q.append(node.right) if node.right else None


tree = BinarySearchTree()
t = int(raw_input())

arr = list(map(int, raw_input().split()))

for i in xrange(t):
	tree.create(arr[i])

levelOrder(tree.root)
