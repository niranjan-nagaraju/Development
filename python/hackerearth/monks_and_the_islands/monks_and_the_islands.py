#encoding: utf-8
'''
https://www.hackerearth.com/practice/algorithms/graphs/breadth-first-search/practice-problems/algorithm/monk-and-the-islands/

Monk and the Islands

Monk visits the land of Islands. There are a total of N islands numbered from 1 to N. Some pairs of islands are connected to each other by Bidirectional bridges running over water.
Monk hates to cross these bridges as they require a lot of efforts. He is standing at Island #1 and wants to reach the Island #N. Find the minimum the number of bridges that he shall have to cross, if he takes the optimal route.

Input:
	First line contains T. T testcases follow.
	First line of each test case contains two space-separated integers N, M.
	Each of the next M lines contains two space-separated integers X and Y , denoting that there is a bridge between Island X and Island Y.

Output:
	Print the answer to each test case in a new line.

Constraints:
	1 ≤ T ≤ 10
	1 ≤ N ≤ 104
	1 ≤ M ≤ 105
	1 ≤ X, Y ≤ N

SAMPLE INPUT
2
3 2
1 2
2 3
4 4
1 2
2 3
3 4
4 2

SAMPLE OUTPUT
2
2
'''

class Graph:
	def __init__(self, nVertices, nEdges):
		self.nVertices = nVertices+1
		self.nEdges = nEdges
		self.adjlist = [[] for _ in xrange(self.nVertices)] # 1-indexed

	@staticmethod
	def read_graph(n, m):
		graph = Graph(n, m)
		for i in xrange(m):
			src, dst = map(int, raw_input().split())
			graph.adjlist[src].append(dst)
			graph.adjlist[dst].append(src)
		return graph


	# return the number of edges in the (minimim) path between src-dst
	def bfs_paths(self, src, dst):
		visited = [False]*self.nVertices
		visited[src] = True
		bfs_q = [(src,0)]

		while bfs_q:
			curr, dist = bfs_q.pop()
			if curr == dst:
				return dist

			for v in self.adjlist[curr]:
				if not visited[v]:
					visited[v] = True
					bfs_q.insert(0, (v, dist+1))

		# dst is not reachable from src
		return 0



def monks_and_islands():
	tc = int(input())
	for i in xrange(tc):
		n,m = map(int, raw_input().split())
		graph = Graph.read_graph(n,m)
		print graph.bfs_paths(1, n)


if __name__ == '__main__':
	monks_and_islands()

