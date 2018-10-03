'''
https://www.hackerrank.com/challenges/tree-top-view/problem
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
self.info (the value of the node)"""

def topView(root):
	if not root:
		return

	q = []
	slist = {}
	max_left_width = 0
	max_right_width = 0

	# Top-view begins with the root node
	q.append((0, root))
	slist[0] = root
	while len(q) != 0:
		width, node = q.pop(0)

		if width < 0:
			if width < max_left_width:
				max_left_width = width
				slist[width] = node
		elif width > 0:
			if width > max_right_width:
				max_right_width = width
				slist[width] = node

		# NOTE: width 0, is root, would already be filled in at root,
		# and in a top-view is not going to replaced

		q.append((width-1, node.left))  if node.left else None
		q.append((width+1, node.right)) if node.right else None

	for i in range(max_left_width, max_right_width+1):
		print slist[i],



tree = BinarySearchTree()
t = int(raw_input())

arr = list(map(int, raw_input().split()))

for i in xrange(t):
	tree.create(arr[i])

topView(tree.root)


'''
Test Executions:
116
37 23 108 59 86 64 94 14 105 17 111 65 55 31 79 97 78 25 50 22 66 46 104 98 81 90 68 40 103 77 74 18 69 82 41 4 48 83 67 6 2 95 54 100 99 84 34 88 27 72 32 62 9 56 109 115 33 15 91 29 85 114 112 20 26 30 93 96 87 42 38 60 7 73 35 12 10 57 80 13 52 44 16 70 8 39 107 106 63 24 92 45 75 116 5 61 49 101 71 11 53 43 102 110 1 58 36 28 76 47 113 21 89 51 19 3
1 2 4 14 23 37 108 111 115 116 83 84 85

6
1 2 5 3 4 6
1 2 5 6
'''

