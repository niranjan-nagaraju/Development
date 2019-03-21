from graph import Graph
from data_structures.sll.sll import SLL

'''
Graphs implemented using Adjacency Lists
'''
class AdjList(Graph):

	def __init__(self, vertices, directed=False):
		Graph.__init__(self, vertices, directed)
		self._adjlists = [SLL() for x in xrange(vertices)]


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
			add_edge_helper(self._adjlists, dst, src, weight)


			
	'''
	DFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	'''
	@staticmethod
	def dfs(adjacency_lists, startvertex, visited, aggregate_fn, *args, **kwargs):
		if visited[startvertex]:
			return

		visited[startvertex] = True
		aggregate_fn(startvertex, *args, **kwargs)

		for v,_ in adjacency_lists[startvertex]:
			AdjList.dfs(adjacency_lists, v, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	'''
	def dfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = AdjList._default_printfn
		visited = [False] * self.vertices

		self.dfs(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	'''
	def dfs_all(self, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = AdjList._default_printfn
		visited = [False] * self.vertices

		for i in xrange(self.vertices):
			self.dfs(self._adjlists, i, visited, aggregate_fn, *args, **kwargs)


