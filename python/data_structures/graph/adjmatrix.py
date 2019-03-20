from graph import Graph

'''
Graphs implemented using Adjacency matrix
'''
class AdjMatrix(Graph):
	def __init__(self, vertices, directed=False):
		Graph.__init__(self, vertices, directed)
		self._adjmatrix = [ [None for x in xrange(vertices)] for x in xrange(vertices) ]


	def __str__(self):
		return str(self._adjmatrix)

	
	'''
	Add an edge from source vertex, src, to destination vertex, dst
	assumes un-weighted by default (in which case weight will be set to 1)
	if the graph is un-directed, an edge from dst->src will also be added
	'''
	def add_edge(self, src, dst, weight=1):
		self._adjmatrix[src][dst] = weight

		if not self.directed:
			self._adjmatrix[dst][src] = weight


