/**
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

*/

#include <assert.h>
#include <vector>
#include <queue>
#include <iostream>
#include <utility>
#include <unordered_map>
#include <algorithm>

// Node for DSU
struct Node {
	int rank;
	int key;
	Node *parent;

	Node(int key) {
		this->key = key;
		this->rank = 0;
		this->parent = 0;
	}
};

// Disjoint set / union-find
class DisjointSet {
	std::unordered_map<int, Node*> nodes_lookup;

public:
	DisjointSet() {
		nodes_lookup = std::unordered_map<int, Node*>();
	}

	~DisjointSet() {
		for (auto node : nodes_lookup)
			delete node.second;
	}

	Node* make_set(int key) {
		if (nodes_lookup.find(key) == nodes_lookup.end()) {
			nodes_lookup[key] = new Node(key);
		}
		return nodes_lookup[key];
	}

	Node* find(Node *nodeitem) {
		if (!nodeitem->parent) {
			return nodeitem;
		}

		nodeitem->parent = this->find(nodeitem->parent);
		return nodeitem->parent;
	}

	Node* find(int item) {
		Node *n = make_set(item);
		return find(n);
	}


	void union_(int src, int dst) {
		Node *lrep, *rrep;

		lrep = find(src);
		rrep = find(dst);

		if (lrep == rrep)
			return;

		if (lrep->rank <= rrep->rank) {
			if (lrep->rank == rrep->rank)
				lrep->rank += 1;
			lrep->parent = rrep;
		} else {
			rrep->parent = lrep;
		}
	}
};


struct Edge {
	int src;
	int dst;
	int distance;

	Edge(int src, int dst, int distance) {
		this->src = src;
		this->dst = dst;
		this->distance = distance;
	}
};


class Graph {
	int nVertices;
	int nEdges;
	std::vector<std::pair<int, int>> *adjlist;
	std::vector<Edge> edges_list;

public:
	Graph(int nVertices, int nEdges) {
		this->nVertices = nVertices;
		this->nEdges = nEdges;
		this->adjlist = new std::vector<std::pair<int, int>>[this->nVertices];
		this->edges_list = std::vector<Edge>();
	}

	~Graph() {
		/** Free allocation for the array of vectors */
		delete[] this->adjlist;
	}


	/** Add a bi-directional edge from src->dst and dst-> src into the adjacency list */
	void add_edge_to_adjlist(int src, int dst, int distance) {
		this->adjlist[src].push_back(std::make_pair(dst, distance));
		this->adjlist[dst].push_back(std::make_pair(src, distance));
	}

	/** Add edge to edges list */
	void add_edge_to_list(int src, int dst, int distance) {
		this->edges_list.push_back(Edge(src, dst, distance));
	}


	/** Read graph edges' endpoints and link them */
	void read_graph()  {
		for (int i=0; i<this->nEdges; i++) {
			int src, dst, distance;

			std::cin >> src >> dst >> distance;
			add_edge_to_list(src, dst, distance);
		}
	}


	/** Print the adjacency list/edges list (whichever is used in the graph backend) of the graph */
	void print(void) {
		std::cout << "Graph: " << std::endl;
		if (this->edges_list.size() != 0) {
			for (Edge e : this->edges_list)
				std::cout << e.src << " " << e.dst << " " << e.distance << std::endl;

			return;
		}

		for (int i=0; i<this->nVertices; i++) {
			std::vector<std::pair<int,int>> &v = this->adjlist[i];
			std::cout << i << ": ";
			for (auto x : v)
				std::cout << "(" << x.first << "," << x.second << ") ";
			std::cout << std::endl;
		}
	}


	/** Generate a Minimum Spanning Tree using Kruskal's algorithm */
	Graph* kruskalsMST(void) {
		Graph *mst = new Graph(this->nVertices, this->nVertices-1);
		DisjointSet dsu;

		int num_edges = 0;
		int total_cost = 0;

		std::vector<Edge> sorted_edges = this->edges_list;
		auto EdgeComparator = [](Edge e1, Edge e2) {
			return (e1.distance < e2.distance);
		};
		std::sort(sorted_edges.begin(), sorted_edges.end(), EdgeComparator);

		for (Edge e : sorted_edges) {
			if (num_edges == (this->nVertices-1))
				break;
	
			if (dsu.find(e.src) != dsu.find(e.dst)) {
				dsu.union_(e.src, e.dst);
				mst->add_edge_to_adjlist(e.src, e.dst, e.distance);
				total_cost += e.distance;
				num_edges++;
			}
		}

		//mst->print();
		// total cost is 2*total weight of the MST
		std::cout<< 2*total_cost << std::endl;

		return mst;
	}

	/** Return the distance between src -> dst using BFS */
	int bfs_distance(int src, int dst) {
		std::queue <std::pair<int, int> > q;
		std::vector<bool> visited(this->nVertices, false);

		visited[src] = true;
		q.push(std::make_pair(src, 0));
		while (!q.empty()) {
			std::pair<int, int> p = q.front();
			q.pop();
			int curr = p.first, dist = p.second;

			if (dst == curr)
				return dist;

			for (auto e : this->adjlist[curr]) {
				int v = e.first;
				int vdist = e.second;
				if (!visited[v]) {
					visited[v] = true;
					q.push(std::make_pair(v, dist+vdist));
				}
			}

		}

		// Completed BFS for all reachable nodes,
		// couldn't find 'dst'
		return -1;
	}


	/** Recursive DFS traversal helper */
	void dfs(int curr, int curr_dist, int dst, int *distance, std::vector<bool> &visited) {
		if (curr == dst) {
			*distance = curr_dist;
			return;
		}

		for (auto e : this->adjlist[curr]) {
			int v = e.first;
			int vdist = e.second;
			if (!visited[v]) {
				visited[v] = true;
				dfs(v, curr_dist+vdist, dst, distance, visited);
			}
		}
	}

	/** Return the distance between src -> dst using DFS */
	int dfs_distance(int src, int dst) {
		std::vector<bool> visited(this->nVertices, false);

		visited[src] = true;
		int distance = 0;

		dfs(src, 0, dst, &distance, visited);

		return distance;
	}
};


int main(void)
{
	int n, m;
	int nQueries;

	std::cin >> n >> m >> nQueries;

	Graph g(n, m);;
	g.read_graph();
	//g.print();

	Graph *mst = g.kruskalsMST();

	while (nQueries--) {
		int src, dst, dist;
		std::cin >> src >> dst;
		dist = mst->bfs_distance(src, dst);

		// An MST should return the same distance between src->dst
		// using both bfs and dfs
		assert (mst->dfs_distance(src, dst) == dist);
		std::cout << dist << std::endl;
	}

	return 0;
}


