# -*- coding: UTF-8 -*-


from data_structures.graph.adjlist import Graph

def basic_testcases():
	g = Graph(3)
	assert str(g) == "[[0]:, [0]:, [0]:]"

	# undirected graph
	g.add_edge(1,2)
	g.add_edge(1,2) # duplicate - won't be added to adjacency lists
	g.add_edge(1,0)
	assert str(g) == "[[1]: (1, None), [2]: (0, None) (2, None), [1]: (1, None)]"

	# directed graph
	g = Graph(3, directed=True)
	g.add_edge(1,2)
	g.add_edge(1,2) # duplicate - won't be added to adjacency lists
	g.add_edge(1,0)
	assert str(g) == "[[0]:, [2]: (0, None) (2, None), [0]:]"


	# weighted undirected graph
	g = Graph(3)
	g.add_edge(1,2, 5)
	g.add_edge(1,2, 10) # duplicate - update weight
	g.add_edge(1,0, 11)
	assert str(g) == "[[1]: (1, 11), [2]: (0, 11) (2, 10), [1]: (1, 10)]"

	# weighted directed graph
	g = Graph(3, directed=True)
	g.add_edge(1,2, 5)
	g.add_edge(1,2, 10) # duplicate - Update weight
	g.add_edge(1,0, 11)
	assert str(g) == "[[0]:, [2]: (0, 11) (2, 10), [0]:]"



'''
DFS on an undirected graph
'''
def test_dfs_undirected(recursive=True):
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
def test_dfs_directed(recursive=True):
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
def test_bfs_undirected(recursive=False):
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
def test_bfs_directed(recursive=False):
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



def test_dfs():
	test_dfs_undirected()
	test_dfs_undirected(recursive=False)
	test_dfs_directed()
	test_dfs_directed(recursive=False)


def test_bfs():
	test_bfs_undirected()
	test_bfs_undirected(recursive=True)
	test_bfs_directed()
	test_bfs_directed(recursive=True)



def test_paths_directed():
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


	g.paths(0, 6, aggregate_list)
	assert aggregate_list.l == [[0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6], [0, 4, 6]]

	aggregate_list.l = []
	g.paths(1, 6, aggregate_list)
	assert aggregate_list.l == [[1, 5, 6]]

	aggregate_list.l = []
	g.paths(6, 1, aggregate_list)
	assert aggregate_list.l == []

	# Try BFS-based all paths extraction
	aggregate_list.l = []
	g.paths_2(0, 6, aggregate_list)
	assert aggregate_list.l == [[0, 4, 6], [0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6]]

	aggregate_list.l = []
	g.paths_2(1, 6, aggregate_list)
	assert aggregate_list.l == [[1, 5, 6]]

	aggregate_list.l = []
	g.paths_2(6, 1, aggregate_list)
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


	
def test_paths_undirected():
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


	g.paths(0, 6, aggregate_list)
	assert aggregate_list.l == [[0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6], [0, 4, 6]]

	aggregate_list.l = []
	g.paths(1, 6, aggregate_list)
	assert aggregate_list.l == [[1, 0, 2, 5, 6], [1, 0, 3, 5, 6], [1, 0, 4, 6], [1, 5, 2, 0, 4, 6], [1, 5, 3, 0, 4, 6], [1, 5, 6]]

	aggregate_list.l = []
	g.paths(1, 1, aggregate_list)
	assert aggregate_list.l == [[1]]

	# Try BFS-based all paths extraction
	aggregate_list.l = []
	g.paths_2(0, 6, aggregate_list)
	assert aggregate_list.l == [[0, 4, 6], [0, 1, 5, 6], [0, 2, 5, 6], [0, 3, 5, 6]]

	aggregate_list.l = []
	g.paths_2(1, 6, aggregate_list)
	assert aggregate_list.l == sorted([[1, 0, 2, 5, 6], [1, 0, 3, 5, 6], [1, 0, 4, 6], [1, 5, 2, 0, 4, 6], [1, 5, 3, 0, 4, 6], [1, 5, 6]],
			cmp=lambda l1,l2: cmp(len(l1), len(l2)))

	aggregate_list.l = []
	g.paths_2(1, 1, aggregate_list)
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



def test_paths():
	test_paths_directed()
	test_paths_undirected()

if __name__ == '__main__':
	basic_testcases()
	test_dfs()
	test_bfs()
	test_paths()

