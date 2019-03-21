from graph import Graph as GraphBase

'''
Graphs implemented using Adjacency matrix
'''
class Graph(GraphBase):
	def __init__(self, vertices, directed=False):
		GraphBase.__init__(self, vertices, directed)
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


	'''
	DFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	'''
	@staticmethod
	def dfs(adjacency_matrix, startvertex, visited, aggregate_fn, *args, **kwargs):
		if visited[startvertex]:
			return

		visited[startvertex] = True
		aggregate_fn(startvertex, *args, **kwargs)

		vertices = len(visited)
		for v in xrange(vertices):
			# if there exists an edge from startvertex to vertex, v
			# continue DFS to vertex v
			if adjacency_matrix[startvertex][v] is not None:
				Graph.dfs(adjacency_matrix, v, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	'''
	def dfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.dfs(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	'''
	def dfs_all(self, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		for i in xrange(self.vertices):
			self.dfs(self._adjmatrix, i, visited, aggregate_fn, *args, **kwargs)


