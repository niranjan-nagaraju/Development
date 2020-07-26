
from data_structures.sll.stack import Stack

class TopologicalSorter(object):
	'''
	Get a topological sort order of vertices given a DAG
	Returns a stack containing the ordering
	'''
	@staticmethod
	def topological_sort(graph):
		# Topological sort is ambiguous on undirected graphs
		if not graph.directed:
			raise TypeError("Topological Sort only works on directed graphs")

		# Do a DFS traversal, adding vertices to the stack
		# once all its neighbors have been visited
		def _topsort_dfs_util(curr_vertex):
			if visited[curr_vertex]:
				return

			visited[curr_vertex] = True
			for v,_ in graph.get_neighbors(curr_vertex):
				_topsort_dfs_util(v)

			# All neighbors of vertex, curr_vertex, have been visited
			# add curr_vertex to stack
			stack.push(curr_vertex)


		# call helper function
		stack = Stack()
		visited = [False] * graph.vertices

		for v in xrange(graph.vertices):
			# Start a DFS-based topological sort at vertex 0,
			# and continue for all vertices not reachable
			# from vertices visited so far
			if not visited[v]:
				_topsort_dfs_util(v)

		return stack





