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

		visited[startvertex] = True
		neighbors.push(startvertex)
		while neighbors:
			v = neighbors.pop()
			aggregate_fn(v, *args, **kwargs)

			vertices = len(visited)
			# push all vertices that are neighbors of v
			# but in reverse order of their vertex indices.
			# so the vertices are ordered in increasing order inside the stack
			for n in xrange(vertices-1, -1, -1):
				# if there exists an edge from v to vertex, n
				# continue DFS to vertex n
				if adjacency_matrix[v][n] is not None and not visited[n]:
					visited[n] = True
					neighbors.push(n)




	'''
	Start a Depth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	Helper to pick between recursive and iterative versions
	'''
	def _dfs_reachable_(self, recursive=True, startvertex=0, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			dfs = self.dfs
		else:
			dfs = self.dfs_i

		visited = [False] * self.vertices
		dfs(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)



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
	def _dfs_all_(self, recursive, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		if recursive:
			dfs = self.dfs
		else:
			dfs = self.dfs_i

		visited = [False] * self.vertices
		for i in xrange(self.vertices):
			if not visited[i]:
				dfs(self._adjmatrix, i, visited, aggregate_fn, *args, **kwargs)



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
	def bfs(adjacency_matrix, startvertex, visited, aggregate_fn, *args, **kwargs):
		neighbors = Queue()

		visited[startvertex] = True
		neighbors.enqueue(startvertex)
		while neighbors:
			v = neighbors.dequeue()
			aggregate_fn(v, *args, **kwargs)

			# enqueue all vertices that are neighbors of v
			for n in xrange(len(visited)):
				if adjacency_matrix[v][n] is not None and not visited[n]:
					visited[n] = True
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
			aggregate_fn(v, *args, **kwargs)

			# enqueue all vertices that are neighbors of v
			for n in xrange(len(visited)):
				if adjacency_matrix[v][n] is not None and not visited[n]:
					visited[n] = True
					neighbors.enqueue(n)

			bfs_helper()

		neighbors = Queue()
		visited[startvertex] = True
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
		bfs(self._adjmatrix, startvertex, visited, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses recursive BFS
	'''
	def bfs_reachable_r(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._bfs_reachable_(True, startvertex, aggregate_fn, *args, **kwargs)


	'''
	Start a Breadth-First search from 'startvertex'
	Traverses only vertices reachable from 'startvertex'
	NOTE: Uses iterative BFS
	'''
	def bfs_reachable(self, startvertex=0, aggregate_fn=None, *args, **kwargs):
		self._bfs_reachable_(False, startvertex, aggregate_fn, *args, **kwargs)




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
				bfs(self._adjmatrix, v, visited, aggregate_fn, *args, **kwargs)


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


	# Return all paths between vertex v1 and vertex v2 using DFS
	def paths(self, v1, v2, aggregate_fn=None, *args, **kwargs):
		def paths_util(prefix, curr_vertex, visited):
			if visited[curr_vertex]:
				return

			# found destination vertex,
			# record this path as one of the paths
			if curr_vertex == v2:
				aggregate_fn(prefix + [v2], *args, **kwargs)

			visited[curr_vertex] = True
			for v in xrange(self.vertices):
				if self._adjmatrix[curr_vertex][v] is not None:
					paths_util(prefix + [curr_vertex], v, visited)

			# Mark current vertex as un-visited
			# so all paths from v1 to v2
			# can be extracted
			visited[curr_vertex] = False

		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		# call helper function
		visited = [False] * self.vertices
		paths_util([], v1, visited)



	# Return all paths between vertex v1 and vertex v2 using BFS
	# should return paths ordered by length of the paths
	def paths_2(self, v1, v2, aggregate_fn=None, *args, **kwargs):
		if not aggregate_fn:
			aggregate_fn = Graph._default_printfn

		q = Queue()
		q.enqueue((v1, []))

		while q:
			curr_vertex, curr_prefix = q.dequeue()

			# found destination vertex,
			# record this path as one of the paths
			if curr_vertex == v2:
				aggregate_fn(curr_prefix + [v2], *args, **kwargs)

			for v in xrange(self.vertices):
				# visited[] traking doesn't yield well to BFS
				# when extracting all paths
				# Instead just check if we don't add a vertex already
				# in the current path so we don't loop endlessly
				# if there's a cycle / its an undirected graph
				if self._adjmatrix[curr_vertex][v] is not None and v not in curr_prefix:
					q.enqueue((v, curr_prefix + [curr_vertex]))




	# Return all the shortest path between vertex v1 and vertex v2 by number of edges in the paths
	def all_shortest_paths_by_length(self, v1, v2, aggregate_fn=None, *args, **kwargs):
		q = Queue()
		q.enqueue((v1, [], 0))

		# Maximum number of vertices wont exceed the number of vertices
		# if there are cycles, they'd not be included more than once in the path
		shortest_path_level = self.vertices

		while q:
			curr_vertex, curr_prefix, level = q.dequeue()

			# Found all the shortest paths
			if level > shortest_path_level:
				return

			# found destination vertex,
			# record this path as one of the paths
			if curr_vertex == v2:
				path = curr_prefix + [v2]
				shortest_path_level = level
				aggregate_fn(path, *args, **kwargs)

			for v in xrange(self.vertices):
				# visited[] traking doesn't yield well to BFS
				# when extracting all paths
				# Instead just check if we don't add a vertex already
				# in the current path so we don't loop endlessly
				# if there's a cycle / its an undirected graph
				if self._adjmatrix[curr_vertex][v] is not None and v not in curr_prefix:
					q.enqueue((v, curr_prefix + [curr_vertex], level+1))



	# Return the shortest path between vertex v1 and vertex v2 by number of edges in the paths
	def shortest_path_by_length(self, v1, v2):
		q = Queue()
		retrace = {}

		q.enqueue(v1)
		retrace[v1] = None

		def retrace_path(retrace):
			path = []
			v = v2
			while v is not None:
				path.insert(0, v)
				v = retrace[v]

			return path


		# Start a BFS traversal
		while q:
			curr_vertex = q.dequeue()

			# found destination vertex,
			# retrace from last vertex using the mapping all the way to v1
			if curr_vertex == v2:
				return retrace_path(retrace)

			for v in xrange(self.vertices):
				# visited[] traking doesn't yield well to BFS
				# when extracting all paths
				# Instead just check if we don't add a vertex already
				# in the current path so we don't loop endlessly
				# if there's a cycle / its an undirected graph
				if self._adjmatrix[curr_vertex][v] is not None and not retrace.has_key(v):
					q.enqueue(v)
					retrace[v] = curr_vertex

		return []



	'''
	Get a topological sort order of vertices given a DAG
	Returns a stack containing the ordering
	'''
	def topological_sort(self):
		# Topological sort is ambiguous on undirected graphs
		if not self.directed:
			raise TypeError

		# Do a DFS traversal, adding vertices to the stack
		# once all its neighbors have been visited
		def _topsort_dfs_util(curr_vertex):
			if visited[curr_vertex]:
				return

			visited[curr_vertex] = True
			for v in xrange(self.vertices):
				if self._adjmatrix[curr_vertex][v] is not None:
					_topsort_dfs_util(v)

			# All neighbors of vertex, curr_vertex, have been visited
			# add curr_vertex to stack
			stack.push(curr_vertex)


		# call helper function
		stack = Stack()
		visited = [False] * self.vertices

		for v in xrange(self.vertices):
			# Start a DFS-based topological sort at vertex 0,
			# and continue for all vertices not reachable
			# from vertices visited so far
			if not visited[v]:
				_topsort_dfs_util(v)

		return stack



