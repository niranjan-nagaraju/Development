from graph import Graph as GraphBase
from data_structures.sll.sll import SLL
from data_structures.sll.queue import Queue
from data_structures.sll.stack import Stack

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
	DFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	iterative version
	'''
	@staticmethod
	def dfs_i(adjacency_lists, startvertex, visited, aggregate_fn, *args, **kwargs):
		neighbors = Stack()
		neighbors.push(startvertex)
		while neighbors:
			v = neighbors.pop()
			if not visited[v]:
				visited[v] = True
				aggregate_fn(v, *args, **kwargs)

				# push all vertices that are neighbors of v
				# NOTE: the adjacency list is ordered by vertex based on their numbers
				# pushing it as-is in the stack will reverse this order
				# Therefore, the recursive and iterative versions
				# will not be in sync wrt to the order of the vertices tracversed.
				for n,_ in adjacency_lists[v]:
					if not visited[n]:
						neighbors.push(n)



	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	Helper to pick between recursive and iterative versions
	'''
	def _dfs_reachable_(self,  recursive=True, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			dfs = self.dfs
		else:
			dfs = self.dfs_i

		visited = [False] * self.vertices

		dfs(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	Recursive version
	'''
	def dfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._dfs_reachable_(True, startvertex, aggregate_fn, *args, **kwargs)

	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	Iterative version
	'''
	def dfs_reachable_i(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._dfs_reachable_(False, startvertex, aggregate_fn, *args, **kwargs)


	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	Helper to pick between recursive and iterative versions
	'''
	def _dfs_all_(self, recursive=True, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			dfs = self.dfs
		else:
			dfs = self.dfs_i

		visited = [False] * self.vertices
		for i in xrange(self.vertices):
			dfs(self._adjlists, i, visited, aggregate_fn, *args, **kwargs)

	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	Recursive version
	'''
	def dfs_all(self, aggregate_fn=None, *args, **kwargs):
		self._dfs_all_(True, aggregate_fn, *args, **kwargs)
		

	'''
	Start a Depth-First search from 'vertex 0'
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	Iterative version
	'''
	def dfs_all_i(self, aggregate_fn=None, *args, **kwargs):
		self._dfs_all_(False, aggregate_fn, *args, **kwargs)


	'''
	BFS traversal of a graph from a specified 'startvertex'
	Tries to reach all vertices reachable from 'startvertex'
	iterative version
	'''
	@staticmethod
	def bfs(adjacency_lists, startvertex, visited, aggregate_fn, *args, **kwargs):
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
	Helper to pick between recursive and iterative versions
	'''
	def _bfs_reachable_(self, recursive=False, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			bfs = self.bfs_r
		else:
			bfs = self.bfs

		visited = [False] * self.vertices
		bfs(self._adjlists, startvertex, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses iterative BFS
	'''
	def bfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._bfs_reachable_(False, startvertex, aggregate_fn, *args, **kwargs)



	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses recursive BFS
	'''
	def bfs_reachable_r(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._bfs_reachable_(True, startvertex, aggregate_fn, *args, **kwargs)



	'''
	Start a Breadth-First from vertex 0
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	Helper to pick between recursive and iterative versions
	'''
	def _bfs_all_(self, recursive=False, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			bfs = self.bfs_r
		else:
			bfs = self.bfs

		visited = [False] * self.vertices
		for v in xrange(self.vertices):
			if not visited[v]:
				bfs(self._adjlists, v, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First from vertex 0
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	NOTE: Uses iterative BFS
	'''
	def bfs_all(self, aggregate_fn=None, *args, **kwargs):
		self._bfs_all_(False, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First from vertex 0
	and then 'vertex 1' to 'vertex n'
	so all vertices are traversed even they are all not connected
	NOTE: Uses recursive BFS
	'''
	def bfs_all_r(self, aggregate_fn=None, *args, **kwargs):
		self._bfs_all_(True, aggregate_fn, *args, **kwargs)


