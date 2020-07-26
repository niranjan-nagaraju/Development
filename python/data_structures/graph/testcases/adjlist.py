# -*- coding: UTF-8 -*-


from data_structures.graph.adjlist import Graph

def basic_testcases():
	g = Graph(3)
	assert str(g) == "[[0]:, [0]:, [0]:]"

	attribute_error_raised = False
	try:
		g.get_adjmatrix()
	except AttributeError:
		attribute_error_raised = True

	# Graph uses adjacency lists for underlying storage
	# and won't have an adjacency matrix
	assert(attribute_error_raised == True)

	# undirected graph
	g.add_edge(1,2)
	g.add_edge(1,2) # duplicate - won't be added to adjacency lists
	g.add_edge(1,0)
	assert str(g) == "[[1]: (1, None), [2]: (0, None) (2, None), [1]: (1, None)]"
	assert str(g) == str(g.get_adjlists())
	assert [v for v,_ in g.get_neighbors(0)] == [1]
	assert [v for v,_ in g.get_neighbors(1)] == [0,2]
	assert [v for v,_ in g.get_neighbors(2)] == [1]


	# directed graph
	g = Graph(3, directed=True)
	g.add_edge(1,2)
	g.add_edge(1,2) # duplicate - won't be added to adjacency lists
	g.add_edge(1,0)
	assert str(g) == "[[0]:, [2]: (0, None) (2, None), [0]:]"
	assert [v for v,_ in g.get_neighbors(0)] == []
	assert [v for v,_ in g.get_neighbors(1)] == [0,2]
	assert [v for v,_ in g.get_neighbors(2)] == []


	# weighted undirected graph
	g = Graph(3)
	g.add_edge(1,2, 5)
	g.add_edge(1,2, 10) # duplicate - update weight
	g.add_edge(1,0, 11)
	assert str(g) == "[[1]: (1, 11), [2]: (0, 11) (2, 10), [1]: (1, 10)]"
	assert [x for x in g.get_neighbors(0)] == [(1,11)]
	assert [x for x in g.get_neighbors(1)] == [(0,11), (2,10)]
	assert [x for x in g.get_neighbors(2)] == [(1,10)]

	# weighted directed graph
	g = Graph(3, directed=True)
	g.add_edge(1,2, 5)
	g.add_edge(1,2, 10) # duplicate - Update weight
	g.add_edge(1,0, 11)
	assert str(g) == "[[0]:, [2]: (0, 11) (2, 10), [0]:]"
	assert [x for x in g.get_neighbors(0)] == []
	assert [x for x in g.get_neighbors(1)] == [(0,11), (2,10)]
	assert [x for x in g.get_neighbors(2)] == []





if __name__ == '__main__':
	basic_testcases()

	from data_structures.graph.testcases.test_graph import GraphTester
	gt = GraphTester(Graph)
	
	gt.test_dfs()
	gt.test_bfs()
	gt.test_paths()
	gt.test_topological_sort()

