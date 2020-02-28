'''
https://www.hackerrank.com/contests/juniper-2016/challenges/aliens-and-portals

Aliens and Portals

You are an engineer who has been abducted by aliens from Mars. The king of Mars needs your help in building portals to travel from one planet to another.
The cost of building a portal between two planets is twice the distance between them. You are given the number of planets N and distances between the planets. You need to find the minimum cost of building portals between planets such that the king can travel from one planet to any of the remaining  N-1 planets by only using the portals built. Also on building the portals, you will have to answer Q queries. In each query you are required to find the actual distance traversed while travelling from one planet to another planet using the portals. Note that the distance between any two planets is distinct.

Note: If the distance between two planets isn't given then a direct portal can't be built between these two planets.

Input Format

The first line contains three integers N, M and Q. N denotes the number of planets and Q denotes the number of queries.
Following  M lines contain 3 integers u,v,dist.  u is source planet id, v is the destination planet id and dist is the distance between the two planets in light years.
Following Q lines contains two integers x and y. You have to determine the distance travelled while moving from planet x to planet y via the portals.

Output Format

The first line contains the minimum total cost of building portals.
Following  lines should contain the answer to each query.

Sample Input
4 5 3  
0 1 10  
0 2 6  
0 3 5  
1 3 15  
2 3 4  
0 1  
0 2  
0 3  
Sample Output
38  
10  
9  
5  
Explanation
0 - 1
| \ |   
2 - 3

MST
0 - 1
 \ 
2 - 3
In this case, 3 portal will be built. One between planet 0 and 1 ,another between planet 0 and 3 and another between planet 2 and 3.
Minimum cost of building the portals is 2*(10+5+4)=38.
For travelling from planet 0 to planet 1 the king would use the portal built between planet 0 and 1.
He would be traversing a distance equal to 10 light years.
For travelling from planet 0 to planet 2, he would have to use the portal built between 0 and 3 and the portal built between 2 and 3.
He would be traversing a distance equal to 9 light years.
For travelling from planet 0 to planet 3, the king would use the portal built between 0 and 3.He would be traversing a distance equal to 5 light years.
'''


# Node for DSU
class Node(object):
	def __init__(self, key):
		self.rank = 0
		self.parent = None
		self.key = key

# Disjoint-set Union
class DSU(object):
	def __init__(self):
		self.nodes_lookup = {}

	def make_set(self, item):
		n = self.nodes_lookup.get(item)
		if n is None:
			n = Node(item)
			self.nodes_lookup[item] = n
		return n


	def find(self, item):
		def find_(node):
			if node.parent == None:
				return node

			node.parent = find_(node.parent)
			return node.parent

		item = self.make_set(item)
		return find_(item)

	def union(self, a, b):
		lrep = self.find(a)
		rrep = self.find(b)

		if lrep == rrep:
			return
		
		if lrep.rank <= rrep.rank:
			if lrep.rank == rrep.rank:
				rrep.rank += 1
			lrep.parent = rrep
		else:
			rrep.parent = lrep


# Graph
class Graph(object):
	def __init__(self, n, m):
		self.nV = n
		self.nE = m
		# Either store list of edges or the adjacency list
		self.adjlist = [[] for _ in xrange(n)]
		self.edges = []

	def add_edge_to_adjlist(self, src, dst, dist):
		self.adjlist[src].append((dst, dist))
		self.adjlist[dst].append((src, dist))

	def add_edge_to_list(self, src, dst, dist):
		self.edges.append((src, dst, dist))

	# print the adjacency list or the list of edges whichever
	# underlying representation the graph object uses
	def print_(self):
		if not self.edges:
			for i,x in enumerate(self.adjlist, 0):
				print i, ':', x
		else:
			for e in self.edges:
				print e
			print  self.edges



	# Generate a Minimum Spanning Tree using Kruskal's algorithm
	def kruskalsMST(self):
		mst = Graph(self.nV, self.nV-1)
		dsu = DSU()

		num_edges = 0
		mst_weight = 0
		# sort edges by their distances
		for e in sorted(self.edges, key=lambda (u,v,d): d):
			if num_edges == self.nV - 1:
				break

			src, dst, dist = e
			if dsu.find(src) != dsu.find(dst):
				dsu.union(src, dst)
				mst.add_edge_to_adjlist(src, dst, dist)
				mst_weight += dist
				num_edges += 1

		# print the cost of building the portals
		# which is twice the distances between the MST edges
		# => 2 * weight of the MST = total cost
		print 2*mst_weight
		#mst.print_()
		return mst


	# Calculate distance between src -> dst using bfs
	def bfs_distance(self, src, dst):
		visited = [False] * self.nV
		q = [(src, 0)]
		while q:
			curr, dist = q.pop(0)
			if curr == dst:
				return dist

			for (v, vdist) in self.adjlist[curr]:
				if not visited[v]:
					q.append((v, dist+vdist))
					visited[v] = True

		# 'If' the graph is disconnected !!
		return -1

	# Calculate distance between src -> dst using dfs
	def dfs_distance(self, src, dst):
		def dfs(curr, curr_dist):
			if curr == dst:
				dist[0] = curr_dist
				return

			for (v, vdist) in self.adjlist[curr]:
				if not visited[v]:
					visited[v] = True
					dfs(v, curr_dist+vdist)
			
		visited = [False] * self.nV
		dist = [0]
		visited[src] = True
		dfs(src, 0)
		return dist[0]






def aliens_and_portals():
	n, m, q = map(int, raw_input().strip().split())
	g = Graph(n, m)

	while m > 0:
		src, dst, dist = map(int, raw_input().strip().split())
		g.add_edge_to_list(src, dst, dist)
		m -= 1

	#g.print_()
	mst = g.kruskalsMST()

	while q > 0:
		src, dst = map(int, raw_input().strip().split())
		dist = mst.bfs_distance(src, dst)

		# An MST should return the same distance between src->dst
		# using both bfs and dfs
		assert dist == mst.dfs_distance(src, dst)
		print dist
		q -= 1


if __name__ == '__main__':
	aliens_and_portals()

