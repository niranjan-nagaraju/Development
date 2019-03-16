'''
'Abstract' class template for a graph
'''
class Graph(object):
	def __init__(self, vertices, directed=False):
		self.vertices = vertices
		self.directed = directed


	# Add an edge from vertex src -> dst
	def add_edge(self, src, dst):
		pass


	@property
	def matrix(self):
		pass


	@property
	def adj_list(self):
		pass


	def bfs(self):
		pass


	def dfs(self):
		pass
