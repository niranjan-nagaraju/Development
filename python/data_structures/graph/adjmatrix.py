from graph import Graph as GraphBase
from data_structures.sll.queue import Queue
from data_structures.sll.stack import Stack

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
	DFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	Iterative version
	'''
	@staticmethod
	def dfs_i(adjacency_matrix, startvertex, visited, aggregate_fn, *args, **kwargs):
		neighbors = Stack()
		neighbors.push(startvertex)
		while neighbors:
			v = neighbors.pop()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

			vertices = len(visited)
			# push all vertices that are neighbors of v
			# but in reverse order of their vertex indices.
			# so the vertices are ordered in increasing order inside the stack
			for n in xrange(vertices-1, -1, -1):
				# if there exists an edge from v to vertex, n
				# continue DFS to vertex n
				if adjacency_matrix[v][n] is not None and not visited[n]:
					neighbors.push(n)



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


	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	Iterative version
	'''
	def dfs_reachable_i(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.dfs_i(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	Iterative version
	'''
	def dfs_all_i(self, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		for i in xrange(self.vertices):
			self.dfs_i(self._adjmatrix, i, visited, aggregate_fn, *args, **kwargs)


	'''
	BFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	iterative version
	'''
	@staticmethod
	def bfs_i(adjacency_matrix, startvertex, visited, aggregate_fn, *args, **kwargs):
		neighbors = Queue()
		neighbors.enqueue(startvertex)
		while neighbors:
			v = neighbors.dequeue()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

				# enqueue all vertices that are neighbors of v
				for n in xrange(len(visited)):
					if adjacency_matrix[v][n] is not None and not visited[n]:
						neighbors.enqueue(n)



	'''
	BFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	recursive version
	'''
	@staticmethod
	def bfs_r(adjacency_matrix, startvertex, visited, aggregate_fn, *args, **kwargs):
		def bfs_helper():
			if not neighbors:
				return

			v = neighbors.dequeue()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

			# enqueue all vertices that are neighbors of v
			for n in xrange(len(visited)):
				if adjacency_matrix[v][n] is not None and not visited[n]:
					neighbors.enqueue(n)

			bfs_helper()

		neighbors = Queue()
		neighbors.enqueue(startvertex)
		bfs_helper()



	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses iterative BFS
	'''
	def bfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.bfs_i(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses recursive BFS
	'''
	def bfs_reachable_r(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.bfs_r(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Breadth-First from vertex 0
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	NOTE: Uses iterative BFS
	'''
	def bfs_all(self, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		for v in xrange(self.vertices):
			if not visited[v]:
				self.bfs_i(self._adjmatrix, v, visited, aggregate_fn, *args, **kwargs)

	'''
	Start a Breadth-First from vertex 0
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	NOTE: Uses recursive BFS
	'''
	def bfs_all_r(self, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		for v in xrange(self.vertices):
			if not visited[v]:
				self.bfs_r(self._adjmatrix, v, visited, aggregate_fn, *args, **kwargs)


