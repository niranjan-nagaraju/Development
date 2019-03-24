from graph import Graph as GraphBase
from data_structures.sll.sll import SLL
from data_structures.sll.queue import Queue

'''
Graphs implemented using Adjacency Lists
'''
class Graph(GraphBase):

	def __init__(self, vertices, directed=False):
		GraphBase.__init__(self, vertices, directed)
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
			Graph.dfs(adjacency_lists, v, visited, aggregate_fn, *args, **kwargs)



	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	'''
	def dfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.dfs(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)



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
			self.dfs(self._adjlists, i, visited, aggregate_fn, *args, **kwargs)


	'''
	BFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	iterative version
	'''
	@staticmethod
	def bfs_i(adjacency_lists, startvertex, visited, aggregate_fn, *args, **kwargs):
		neighbors = Queue()
		neighbors.enqueue(startvertex)
		while neighbors:
			v = neighbors.dequeue()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

				# enqueue all vertices that are neighbors of v
				for n,_ in adjacency_lists[v]:
					if not visited[n]:
						neighbors.enqueue(n)



	'''
	BFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	recursive version
	'''
	@staticmethod
	def bfs_r(adjacency_lists, startvertex, visited, aggregate_fn, *args, **kwargs):
		def bfs_helper():
			if not neighbors:
				return

			v = neighbors.dequeue()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

			# enqueue all vertices that are neighbors of v
			for n,_ in adjacency_lists[v]:
				if not visited[n]:
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

		self.bfs_i(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses recursive BFS
	'''
	def bfs_reachable_r(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn
		visited = [False] * self.vertices

		self.bfs_r(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)



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
				self.bfs_i(self._adjlists, v, visited, aggregate_fn, *args, **kwargs)

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
				self.bfs_i(self._adjlists, v, visited, aggregate_fn, *args, **kwargs)


