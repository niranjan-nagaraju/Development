from data_structures.graph.adjmatrix import AdjMatrix as Graph


def basic_testcases():
	g = Graph(3)
	print g

	g.add_edge(1,2)
	g.add_edge(1,0)
	print g


if __name__ == '__main__':
	basic_testcases()
