'''
Dijkstra's algorithm to find single-source shortest paths to
either all other vertices in the graph
or
a single end-point vertex (and possibly to other intermediate vertices in the paths to end-point vertes)
'''


'''
Algorithm Outline:
    0. Let S: start vertex, E: end vertex
    1. Start with adding (startvertex, 0, None) into an Indexed Priority Queue, IPQ, (ordered by distance from S)
    2. If the IPQ is not empty,
        2.1 Dequeue next node(v, distance, from) from IPQ with the smallest distance to S (of all nodes in the IPQ)
        2.2 Mark dequeued vertex, v as visited.
        2.3 For every neighbor n of v
            2.3.1 If n is not is the IPQ,
                    Enqueue (n, (distance of node from S + distance of n from v), node) into IPQ
            2.3.2 If n is already in IPQ,
                    Use IPQ's decreasekey() to update n's distance from S if 
                      (distance of node from S + distance of n from v) < (current distance of n from S)
            2.3.3 NOTE: It's not necessary to initialize the IPQ with all nodes with INFINITY as their distance from S
                  find() and enqueue() if a vertex is missing from IPQ obviates the need for this.
        2.4 Store {distance, from} for v in a lookup table to reconstruct path to S
        2.5 If E is specified, and v == E, we are done finding the shortest path / distance to E, reconstruct path to S
             Otherwise, repeat 2. until IPQ is empty
             Use lookup table to reconstruct paths to all reachable vertices from S


Sample run:
          2
    (S) ------> (A) 
    |     +-----/ |
  4 |    /        | 5
    |   / 1       |
    v  v          v
    (B) --------> (E)
           2


IPQ: (S, 0, -)
Distances: []
visited: []

Dequeue: (S, 0, -)
    visited: [S]
    Distances: {
      S: 0
    }
    Neighbors: {A, B}
    Add (A, 2, S), (B, 4, S) tp IPQ
    IPQ: (A, 2, S) -> (B, 4, S)

Dequeue: (A, 2, S)
    visited: [S, A]
    Distances: {
      S: 0
      A: 2 <- S
    }
    Neighbors: {B, E}
    Add (B, 3, A), (E, 7, A) to IPQ
    IPQ: (B, 3, A) -> (E, 7, A)

Dequeue: (B, 3, A)
    visited: [S, A, B]
    Distances: {
      S: 0
      A: 2 [S]
      B: 3 [S -> A]
    }
    Neighbors: {E}
    Add (E, 5, B) to IPQ
    IPQ: (E, 5, B)

Dequeue: (E, 5, B)
    visited: [S, A, B, E]
    Distances: {
      S: 0
      A: 2 [S -> A]
      B: 3 [S -> A -> B]
      E: 5 [S -> A -> B -> E]
    }
    E: Found end vertex
    shortest path to E from S: 5 [S -> A -> B -> E]
'''

from data_structures.heap.indexed_priority_queue import IndexedPriorityQueue

class Dijkstras(object):
	class NodeItem(object):
		def __init__(self, vertex, distance, from_):
			self.vertex = vertex
			self.distance = distance
			self.from_ = from_

		# Order nodes in priority queue by their distances from start-vertex
		def __cmp__(self, other):
			return cmp(self.distance, other.distance)

		# Two nodes are the same if they share the same internal state
		def __eq__(self, other):
			return (self.vertex, self.distance, self.from_) == (other.vertex, other.distance, other.from_)

		# Lookup IPQ by vertex
		def __hash__(self):
			return hash(self.vertex)

		def __str__(self):
			return str((self.vertex, self.distance, self.from_))


	'''
	Run Dijkstras algorithm on a graph
	and return a lookup table of (shortest) distances, path breadcrumbs
	to a specified destination vertex(dst) from source vertex(src), and possibly 
	shortest distances to a few intermediate vertices.

	If no destination is specified, Shortest distances (by weight)
	will be calculated to all reachable vertices from 'src' and returned.
	'''
	@staticmethod
	def sssp(graph, src, dst=None):
		shortest_distances = {}

		ipq = IndexedPriorityQueue()
		ipq.enqueue(Dijkstras.NodeItem(src, 0, None))

		while ipq:
			node = ipq.dequeue()
			shortest_distances[node.vertex] = (node.distance, node.from_)

			if dst is not None and dst == node.vertex:
				# Found shortest path to 'end' vertex
				return shortest_distances

			for n,dist in graph.get_neighbors(node.vertex):
				if shortest_distances.get(n) is not None:
					# vertex, n, is already done/visited.
					continue

				if ipq.find(n) is None:
					# vertex 'n' is not in the priority queue
					# Add it with the (distance{S->node.vertex} + distance{node.vertex->n})
					ipq.enqueue(Dijkstras.NodeItem(n, node.distance + dist, node.vertex))
				else:
					ni = ipq.find(n)
					if ni.distance > node.distance + dist:
						# path to 'n' via 'node.vertex' is shorter
						# than the currently calculated path to 'n'
						ipq.promote(ni, Dijkstras.NodeItem(n, node.distance+dist, node.vertex))

		return shortest_distances


	'''
	Extract shortest distance path to 'dst' from 'src'
	using a table returned by Dijkstras
	'''
	@staticmethod
	def extract_path(shortest_distances_tbl, dst):
		stack = []
		vertex = dst
		while vertex is not None:
			stack.append(vertex)
			_, vertex = shortest_distances_tbl[vertex]

		return stack[::-1]


	'''
	Return the shortest distance to 'dst' vertex from a 'src' vertex
	usng a table returned by a previously-application of Dijkstras
	'''
	@staticmethod
	def extract_distance(shortest_distances_tbl, dst):
		node = shortest_distances_tbl.get(dst)
		if node is None:
			return None
		return node[0]



