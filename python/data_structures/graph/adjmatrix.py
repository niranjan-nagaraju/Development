from graph import Graph

class AdjMatrix(Graph):
	def __init__(self, vertices, directed=False):
		Graph.__init__(self, vertices, directed)
		self._adjmatrix = [ [None for x in xrange(vertices)] for x in xrange(vertices) ]


	def __str__(self):
		return str(self._adjmatrix)

	def add_edge(self, src, dst, weight=1):
		self._adjmatrix[src][dst] = weight

		if not self.directed:
			self._adjmatrix[dst][src] = weight


