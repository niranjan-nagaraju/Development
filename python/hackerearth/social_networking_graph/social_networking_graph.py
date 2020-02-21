'''
https://www.hackerearth.com/practice/algorithms/graphs/breadth-first-search/practice-problems/algorithm/social-networking-graph/

Social Networking Graph

In a social networking site, people are connected with other people. The whole system appears as a giant connected graph. In this question, you are required to answer the total number of people connected at t nodes away from each other (t distance connectivity). For example: Two persons directly connected are at 1 distance connectivity. While the two persons having a common contact without having direct connectivity, are at 2 distance connectivity.

First line of input line contains, two integers n and e, where n is the nodes and e are the edges. Next e line will contain two integers u and v meaning that node u and node v are connected to each other in undirected fashion. Next line contains single integer, m, which is number of queries. Next m lines, each have two inputs, one as source node and other as a required t distance connectivity which should be used to process query.


Note: The index of nodes will be 0-based. The example and the test case shown is of 1-based index. For submitting the solution, use 0-based indexing.


Sample Input
9 10
1 2
2 3
1 7
2 4
3 4
4 7
7 8
9 7
7 6
5 6
3
4 2
5 3
2 1

Sample Output
4
4
3

Graph:
   1 - 2 - 3
    \   \  /
6 -  7 - 4
|   / \
5  9   8

Query 1: (4,2): Number of nodes at distance 2 from 4
 4 - 2 - 1
 4 - 3 - 2  [X] 2 is at a distance of 1 from 4
 4 - 7 - 6
 4 - 7 - 9
 4 - 7 - 8
 => 4 nodes at distance 2

Query 2: (5, 3)
 5 - 6 - 7 - 4
 5 - 6 - 7 - 8
 5 - 6 - 7 - 9
 5 - 6 - 7 - 1
 => 4 nodes at distance 3

 Query 3: (2, 1)
 2 - 1
 2 - 3
 2 - 4
 => 3 nodes at distance 1
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


	def print_(self):
		for i,x in enumerate(self.adjlist, 0):
			print i, ':', x


	# return the number of nodes which are at 'distance' nodes away from 'src'
	def bfs_distances(self, src, distance):
		visited = [False]*self.nVertices
		visited[src] = True
		bfs_q = [(src,0)]
		count = 0
		while bfs_q:
			curr, dist = bfs_q.pop()

			# We are done with all nodes at required distance
			# return nodes counted so far
			if dist > distance:
				return count
			elif dist == distance:
				# Count all nodes at distance,
				count += 1

			for v in self.adjlist[curr]:
				if not visited[v]:
					visited[v] = True
					bfs_q.insert(0, (v, dist+1))

		# Completed BFS for all reachable nodes,
		# return count if the distance was for the last reachable level from src
		return count



def social_networking_graph():
	n,m = map(int, raw_input().split())
	graph = Graph.read_graph(n,m)
	#graph.print_()
	
	nqueries = int(input())
	for q in xrange(nqueries):
		src, dist = map(int, raw_input().split())
		print graph.bfs_distances(src, dist)


if __name__ == '__main__':
	social_networking_graph()

