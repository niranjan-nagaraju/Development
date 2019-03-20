from data_structures.graph.adjlist import AdjList as Graph

def basic_testcases():
	g = Graph(3)
	assert str(g) == "[[0]:, [0]:, [0]:]"

	g.add_edge(1,2)
	g.add_edge(1,2)
	g.add_edge(1,0)
	assert str(g) == "[[1]: (1, None), [2]: (0, None) (2, None), [1]: (1, None)]"


'''
DFS on an undirected graph
'''
def test_dfs_undirected():
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

	g.dfs_reachable(2, aggregate_list)
	assert(aggregate_list.l == [2,0,1,4,3])

	aggregate_list.l = []
	g.dfs_reachable(0, aggregate_list)
	assert(aggregate_list.l == [0,1,2,4,3])

	aggregate_list.l = []
	g.dfs_all(aggregate_list)
	assert(aggregate_list.l == [0,1,2,4,3,5])

	aggregate_list.l = []
	g.dfs_reachable(4, aggregate_list)
	assert(aggregate_list.l == [4,2,0,1,3])

	aggregate_list.l = []
	g.dfs_reachable(5, aggregate_list)
	assert(aggregate_list.l == [5])



'''
DFS on an directed graph
'''
def test_dfs_directed():
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

	g.dfs_reachable(2, aggregate_list)
	assert(aggregate_list.l == [2,0,1])

	aggregate_list.l = []
	g.dfs_reachable(0, aggregate_list)
	assert(aggregate_list.l == [0,1,2])

	aggregate_list.l = []
	g.dfs_all(aggregate_list)
	assert(aggregate_list.l == [0,1,2,3,4,5])

	aggregate_list.l = []
	g.dfs_reachable(4, aggregate_list)
	assert(aggregate_list.l == [4,2,0,1])

	aggregate_list.l = []
	g.dfs_reachable(5, aggregate_list)
	assert(aggregate_list.l == [5])



def test_dfs():
	test_dfs_undirected()
	test_dfs_directed()


if __name__ == '__main__':
	basic_testcases()
	test_dfs()

