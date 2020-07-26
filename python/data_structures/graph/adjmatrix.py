from graph import Graph as GraphBase

'''
Graphs implemented using Adjacency matrix
'''
class Graph(GraphBase):
	def __init__(self, vertices, directed=False):
		GraphBase.__init__(self, vertices, directed)
		self._adjmatrix = [ [None for _ in xrange(vertices)] for _ in xrange(vertices) ]


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
			# Add an edge from (dst -> src) as well if the graph is not directed.
			self._adjmatrix[dst][src] = weight



	'''
	Get neighbors of a specified vertex
	Will yield (v,w) [v: vertex, w: weight] pairs one for each call
	'''
	def get_neighbors(self, vertex):
		for v in xrange(self.vertices):
			if self._adjmatrix[vertex][v] is not None:
				yield (v, self._adjmatrix[vertex][v])


