'''
Disjoint-sets/Union-Find
  Use union by ranking, and path-compression optimizations to reduce the levels of lookup
'''
from node import Node
from collections import defaultdict

'''
Disjoint-set using union by rank and path-compression
'''
class DisjointSet(object):
	def __init__(self):
		# maps data to their respective nodes in the disjoint set
		# for quick lookup
		self.nodes_map = defaultdict(lambda: None)

	
	'''
	Make a singleton set containing 'value'
	'''
	def make_set(self, value):
		if self.nodes_map[value] is None:
			return
		self.nodes_map[value] = Node(value)


	'''
	Merge two sets containing lvalue, and rvalue respectively
	into one
	'''
	def union(self, lvalue, rvalue):
		pass


	'''
	Find set containing a specified value
	'''
	def find(self, value):
		def find_(node):
			if node.parent == None:
				return node
			node.parent = find_(node.parent)
			return node.parent


