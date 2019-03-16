from graph import Graph
from data_structures.sll.sll import SLL

class AdjList(Graph):
	def __init__(self, vertices, directed=False):
		Graph.__init__(self, vertices, directed)
		self._adjlists = [SLL() for x in xrange(vertices)]


	def __str__(self):
		return str(self._adjlists)


	def add_edge(self, src, dst, weight=None):
		self._adjlists[src].place((dst, weight))

		if not self.directed:
			self._adjlists[dst].place((src, weight))




