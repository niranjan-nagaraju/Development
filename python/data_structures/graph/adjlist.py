from graph import Graph as GraphBase
from data_structures.sll.sll import SLL

'''
Graphs implemented using Adjacency Lists
'''
class Graph(GraphBase):

	def __init__(self, vertices, directed=False):
		GraphBase.__init__(self, vertices, directed)
		self._adjlists = [SLL() for _ in xrange(vertices)]


	def __str__(self):
		return str(self._adjlists)


	'''
	Add an edge from source vertex, src, to destination vertex, dst
	assumes un-weighted by default (in which case weight will be set to 1)
	if the graph is un-directed, an edge from dst->src will also be added
	'''
	def add_edge(self, src, dst, weight=None):
		def add_edge_helper(adjlists, v1, v2, weight):
			node = adjlists[v1].findMatchingNode((v2, weight), 
					comparatorfn=lambda (a,w1),(c,w2): cmp(a, c))

			# edge already exists, update weight and return
			if node:
				node.value = (v2, weight)
				return

			adjlists[v1].place((v2, weight), 
				comparatorfn=lambda (a,w1),(c,w2): cmp(a, c), allowduplicates=False)

		add_edge_helper(self._adjlists, src, dst, weight)
		if not self.directed:
			# Add an edge from (dst -> src) as well if the graph is not directed.
			add_edge_helper(self._adjlists, dst, src, weight)



	'''
	Get neighbors of a specified vertex
	Will yield (v,w) [v: vertex, w: weight] pairs one for each call
	'''
	def get_neighbors(self, vertex):
		for v,w in self._adjlists[vertex]:
			yield (v,w)

			

