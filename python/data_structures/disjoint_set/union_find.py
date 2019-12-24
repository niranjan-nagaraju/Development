'''
Disjoint-sets/Union-Find
  Use union by ranking, and path-compression optimizations to reduce the levels of lookup
'''

class DisjointSet(object):
	def __init__(self):
		mapping = {}
