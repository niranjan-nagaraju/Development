# -*- coding: UTF-8 -*-

'''
Tester for a specified graph type
Graph type can be either Adjacency-lists based or Adjacency-matrix-based
'''
class GraphTester(object):
	def __init__(self, graphType):
		self.graphType = graphType


	'''
	DFS on an undirected graph
	'''
	def test_dfs_undirected(self, recursive=True):
		Graph = self.graphType
		g = Graph(6) 
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(1, 2) 
		g.add_edge(2, 0) 
		g.add_edge(3, 3)
		g.add_edge(3, 4)
		g.add_edge(4, 4)
		g.add_edge(4, 2)
		g.add_edge(5, 5)

		'''
		0 -- 1
		 \	 |
		  -- 2 -- |
				  |
		3 -- 4 -- |

		5
		'''

		def aggregate_list(v):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(v)
			return aggregate_list.l

		if recursive:
			dfs_reachable = g.dfs_reachable
			dfs_all = g.dfs_all
		else:
			dfs_reachable = g.dfs_reachable_i
			dfs_all = g.dfs_all_i


		dfs_reachable(2, aggregate_list)
		if recursive:
			assert(aggregate_list.l == [2,0,1,4,3])
		else:
			assert(aggregate_list.l == [2, 4, 3, 1, 0])

		aggregate_list.l = []
		dfs_reachable(0, aggregate_list)
		if recursive:
			assert(aggregate_list.l == [0,1,2,4,3])
		else:
			assert(aggregate_list.l == [0,2,4,3,1])

		aggregate_list.l = []
		dfs_reachable(4, aggregate_list)
		if recursive:
			assert(aggregate_list.l == [4,2,0,1,3])
		else:
			assert(aggregate_list.l == [4,3,2,1,0])

		aggregate_list.l = []
		dfs_reachable(5, aggregate_list)
		assert(aggregate_list.l == [5])

		aggregate_list.l = []
		dfs_all(aggregate_list)
		if recursive:
			assert(aggregate_list.l == [0,1,2,4,3,5])
		else:
			assert(aggregate_list.l == [0,2,4,3,1,5])



	'''
	DFS on an directed graph
	'''
	def test_dfs_directed(self, recursive=True):
		Graph = self.graphType

		g = Graph(6, directed=True) 
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(1, 2) 
		g.add_edge(2, 0) 
		g.add_edge(3, 3)
		g.add_edge(3, 4)
		g.add_edge(4, 4)
		g.add_edge(4, 2)
		g.add_edge(5, 5)

		'''
		0 -> 1
		|    |
		|  	 v
	   < - > 2 <- |
				  |
		3 -> 4 -> |

		5
		'''

		def aggregate_list(v):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(v)
			return aggregate_list.l

		if recursive:
			dfs_reachable = g.dfs_reachable
			dfs_all = g.dfs_all
		else:
			dfs_reachable = g.dfs_reachable_i
			dfs_all = g.dfs_all_i


		dfs_reachable(2, aggregate_list)
		assert(aggregate_list.l == [2,0,1])

		aggregate_list.l = []
		dfs_reachable(0, aggregate_list)
		if recursive:
			assert(aggregate_list.l == [0,1,2])
		else:
			assert(aggregate_list.l == [0,2,1])

		aggregate_list.l = []
		dfs_reachable(4, aggregate_list)
		assert(aggregate_list.l == [4,2,0,1])

		aggregate_list.l = []
		dfs_reachable(5, aggregate_list)
		assert(aggregate_list.l == [5])

		aggregate_list.l = []
		dfs_all(aggregate_list)
		if recursive:
			assert(aggregate_list.l == [0,1,2,3,4,5])
		else:
			assert(aggregate_list.l == [0,2,1,3,4,5])


	'''
	BFS on an undirected graph
	'''
	def test_bfs_undirected(self, recursive=False):
		Graph = self.graphType

		g = Graph(6) 
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(1, 2) 
		g.add_edge(2, 0) 
		g.add_edge(3, 3)
		g.add_edge(3, 4)
		g.add_edge(4, 4)
		g.add_edge(4, 2)
		g.add_edge(5, 5)

		'''
		0 -- 1
		 \	 |
		  -- 2 -- |
				  |
		3 -- 4 -- |

		5
		'''

		def aggregate_list(v):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(v)
			return aggregate_list.l


		if recursive:
			bfs_reachable = g.bfs_reachable_r
			bfs_all = g.bfs_all_r
		else:
			bfs_reachable = g.bfs_reachable
			bfs_all = g.bfs_all

		bfs_reachable(2, aggregate_list)
		assert(aggregate_list.l == [2,0,1,4,3])

		aggregate_list.l = []
		bfs_reachable(0, aggregate_list)
		assert(aggregate_list.l == [0,1,2,4,3])

		aggregate_list.l = []
		bfs_reachable(4, aggregate_list)
		assert(aggregate_list.l == [4,2,3,0, 1])

		aggregate_list.l = []
		bfs_reachable(5, aggregate_list)
		assert(aggregate_list.l == [5])

		aggregate_list.l = []
		bfs_all(aggregate_list)
		assert(aggregate_list.l == [0,1,2,4,3,5])



	'''
	BFS on an directed graph
	'''
	def test_bfs_directed(self, recursive=False):
		Graph = self.graphType

		g = Graph(6, directed=True) 
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(1, 2) 
		g.add_edge(2, 0) 
		g.add_edge(3, 3)
		g.add_edge(3, 4)
		g.add_edge(4, 4)
		g.add_edge(4, 2)
		g.add_edge(5, 5)

		'''
		0 -> 1
		|    |
		|  	 v
	   < - > 2 <- |
				  |
		3 -> 4 -> |

		5
		'''

		def aggregate_list(v):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(v)
			return aggregate_list.l

		if recursive:
			bfs_reachable = g.bfs_reachable_r
			bfs_all = g.bfs_all_r
		else:
			bfs_reachable = g.bfs_reachable
			bfs_all = g.bfs_all

		bfs_reachable(2, aggregate_list)
		assert(aggregate_list.l == [2,0,1])

		aggregate_list.l = []
		bfs_reachable(0, aggregate_list)
		assert(aggregate_list.l == [0,1,2])

		aggregate_list.l = []
		bfs_reachable(4, aggregate_list)
		assert(aggregate_list.l == [4,2,0,1])

		aggregate_list.l = []
		bfs_reachable(5, aggregate_list)
		assert(aggregate_list.l == [5])

		aggregate_list.l = []
		bfs_all(aggregate_list)
		assert(aggregate_list.l == [0,1,2,3,4,5])



	def test_dfs(self):
		self.test_dfs_undirected()
		self.test_dfs_undirected(recursive=False)
		self.test_dfs_directed()
		self.test_dfs_directed(recursive=False)


	def test_bfs(self):
		self.test_bfs_undirected()
		self.test_bfs_undirected(recursive=True)
		self.test_bfs_directed()
		self.test_bfs_directed(recursive=True)



	def test_paths_directed(self):
		Graph = self.graphType

		g = Graph(7, directed=True)
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(0, 3) 
		g.add_edge(0, 4) 
		g.add_edge(1, 5) 
		g.add_edge(2, 5)
		g.add_edge(3, 5)
		g.add_edge(5, 6)
		g.add_edge(4, 6)

		'''
			1
		  ↗   ↘ 
		0 → 2 → 5 → 6
		↓↘     ↗    ↑  
		↓   3     ↗  
		↘       ↗ 
		  4 → ↗ 

		'''

		def aggregate_list(path):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(path)
			return aggregate_list.l


		g.paths_dfs(0, 6, aggregate_list)
		assert aggregate_list.l == [[0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6], [0, 4, 6]]

		aggregate_list.l = []
		g.paths_dfs(1, 6, aggregate_list)
		assert aggregate_list.l == [[1, 5, 6]]

		aggregate_list.l = []
		g.paths_dfs(6, 1, aggregate_list)
		assert aggregate_list.l == []

		# Try BFS-based all paths extraction
		aggregate_list.l = []
		g.paths_bfs(0, 6, aggregate_list)
		assert aggregate_list.l == [[0, 4, 6], [0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6]]

		aggregate_list.l = []
		g.paths_bfs(1, 6, aggregate_list)
		assert aggregate_list.l == [[1, 5, 6]]

		aggregate_list.l = []
		g.paths_bfs(6, 1, aggregate_list)
		assert aggregate_list.l == []

		# All shortest paths by length
		aggregate_list.l = []
		g.all_shortest_paths_by_length(0, 6, aggregate_list)
		assert aggregate_list.l == [[0,4,6]]

		aggregate_list.l = []
		g.all_shortest_paths_by_length(0, 5, aggregate_list)
		assert aggregate_list.l == [[0,1,5], [0,2,5], [0,3,5]]

		# shortest path by length
		assert g.shortest_path_by_length(0, 6) == [0,4,6]
		assert g.shortest_path_by_length(0, 5) == [0,1,5]
		assert g.shortest_path_by_length(6, 1) == []


		
	def test_paths_undirected(self):
		Graph = self.graphType

		g = Graph(7, directed=False)
		g.add_edge(0, 1) 
		g.add_edge(0, 2) 
		g.add_edge(0, 3) 
		g.add_edge(0, 4) 
		g.add_edge(1, 5) 
		g.add_edge(2, 5)
		g.add_edge(3, 5)
		g.add_edge(5, 6)
		g.add_edge(4, 6)

		'''
			1
		  ⤢   ⤡
		0 ⇿  2 ⇿  5 ⇿  6
		↕⤡    ⤢      ⤢  
		↕   3      ⤢
		⤡        ⤢ 
		  4 ⇿  ⤢ 

		'''

		def aggregate_list(path):
			if not hasattr(aggregate_list, "l"):
				setattr(aggregate_list, "l", [])
			aggregate_list.l.append(path)
			return aggregate_list.l


		g.paths_dfs(0, 6, aggregate_list)
		assert aggregate_list.l == [[0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6], [0, 4, 6]]

		aggregate_list.l = []
		g.paths_dfs(1, 6, aggregate_list)
		assert aggregate_list.l == [[1, 0, 2, 5, 6], [1, 0, 3, 5, 6], [1, 0, 4, 6], [1, 5, 2, 0, 4, 6], [1, 5, 3, 0, 4, 6], [1, 5, 6]]

		aggregate_list.l = []
		g.paths_dfs(1, 1, aggregate_list)
		assert aggregate_list.l == [[1]]

		# Try BFS-based all paths extraction
		aggregate_list.l = []
		g.paths_bfs(0, 6, aggregate_list)
		assert aggregate_list.l == [[0, 4, 6], [0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6]]

		aggregate_list.l = []
		g.paths_bfs(1, 6, aggregate_list)
		assert aggregate_list.l == sorted([[1, 0, 2, 5, 6], [1, 0, 3, 5, 6], [1, 0, 4, 6], [1, 5, 2, 0, 4, 6], [1, 5, 3, 0, 4, 6], [1, 5, 6]],
				cmp=lambda l1,l2: cmp(len(l1), len(l2)))

		aggregate_list.l = []
		g.paths_bfs(1, 1, aggregate_list)
		assert aggregate_list.l == [[1]]

		# All shortest paths by length
		aggregate_list.l = []
		g.all_shortest_paths_by_length(0, 6, aggregate_list)
		assert aggregate_list.l == [[0,4,6]]

		aggregate_list.l = []
		g.all_shortest_paths_by_length(1, 6, aggregate_list)
		assert aggregate_list.l == [[1,5,6]]

		aggregate_list.l = []
		g.all_shortest_paths_by_length(0, 5, aggregate_list)
		assert aggregate_list.l == [[0,1,5], [0,2,5], [0,3,5]]

		# shortest path by length
		assert g.shortest_path_by_length(0, 6) == [0,4,6]
		assert g.shortest_path_by_length(1, 6) == [1,5,6]
		assert g.shortest_path_by_length(0, 5) == [0,1,5]


	def test_paths(self):
		self.test_paths_directed()
		self.test_paths_undirected()


	'''
	Test toplogical sort on DAGs
	'''
	def test_topological_sort(self):
		Graph = self.graphType

		g = Graph(7, directed=True)
		g.add_edge(0, 1)
		g.add_edge(0, 2)
		g.add_edge(0, 3)
		g.add_edge(0, 4)
		g.add_edge(1, 5)
		g.add_edge(2, 5)
		g.add_edge(3, 5)
		g.add_edge(5, 6)
		g.add_edge(4, 6)

		'''
			1
		  ↗   ↘ 
		0 → 2 → 5 → 6
		↓↘     ↗    ↑  
		↓   3     ↗  
		↘       ↗ 
		  4 → ↗ 
		'''
		s = g.topological_sort()
		l = [0, 4, 3, 2, 1, 5, 6]
		for i in xrange(7):
			assert s.pop() == l[i]

		'''
		0     1
		 ↘  ↙ |
		   2  |
		 ↙    ↓
		3     4
		 ↘  ↙ 
		   5
		   ↓
		   6
		'''
		g = Graph(7, directed=True)
		g.add_edge(0, 2)
		g.add_edge(1, 2)
		g.add_edge(1, 4)
		g.add_edge(2, 3)
		g.add_edge(3, 5)
		g.add_edge(4, 5)
		g.add_edge(5, 6)

		assert str(g.topological_sort()) == "[7]: 1 4 0 2 3 5 6"


