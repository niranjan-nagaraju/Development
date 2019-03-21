'''
'Abstract' class template for a graph
'''
class Graph(object):
	def __init__(self, vertices, directed=False):
		self.vertices = vertices
		self.directed = directed


	# A default print function if no aggregator is provided
	# for traversal functions
	@staticmethod
	def _default_printfn(vertex):
		print str(vertex) + " ",


	# Add an edge from vertex src -> dst
	def add_edge(self, src, dst):
		pass


	@property
	def matrix(self):
		pass


	@property
	def adj_list(self):
		pass


	def bfs(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		pass


	def dfs(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		pass
