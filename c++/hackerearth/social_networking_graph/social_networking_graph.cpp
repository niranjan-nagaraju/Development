/**
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
*/

#include <assert.h>
#include <vector>
#include <queue>
#include <iostream>
#include <utility>

class Graph {
	int nVertices;
	int nEdges;
	std::vector<int> *adjlist;

	public:
		Graph(int nVertices, int nEdges) {
			this->nVertices = nVertices+1; // 1-indexed, 0 is unused
			this->nEdges = nEdges;
			this->adjlist = new std::vector<int>[this->nVertices];
		}

		~Graph() {
			/** Free allocation for the array of vectors */
			delete[] this->adjlist;
		}


		/** Add a bi-directional edge from src->dst and dst-> src */
		void add_edge(int src, int dst) {
			this->adjlist[src].push_back(dst);
			this->adjlist[dst].push_back(src);
		}


		/** Read graph edges' endpoints and link them */
		void read_graph()  {
			for (int i=0; i<this->nEdges; i++) {
				int src, dst;

				std::cin >> src >> dst;
				add_edge(src, dst);
			}
		}


		/** Print the adjacency list of the graph */
		void print(void) {
			std::cout << "Graph: " << std::endl;
			for (int i=0; i<this->nVertices; i++) {
				std::vector<int> &v = this->adjlist[i];
				std::cout << i << ": ";
				for (auto x : v)
					std::cout << x << " ";
				std::cout << std::endl;
			}
		}

		/** return the number of nodes which are at 'distance' nodes away from 'src' */
		int bfs_distances(int src, int distance) {
			std::queue <std::pair<int, int> > q;
			std::vector<bool> visited(this->nVertices, false);
			int count = 0;

			visited[src] = true;
			q.push(std::make_pair(src, 0));
			while (!q.empty()) {
				std::pair<int, int> p = q.front();
				q.pop();
				int curr = p.first, dist = p.second;

				// We are done with all nodes at required distance
				// return nodes counted so far
				if (dist > distance)
					return count;
				else if (dist == distance) // Count all nodes at distance,
					count += 1;

				for (int v : this->adjlist[curr]) {
					if (!visited[v]) {
						visited[v] = true;
						q.push(std::make_pair(v, dist+1));
					}
				}

			}

			// Completed BFS for all reachable nodes,
			// return count if the distance was for the last reachable level from src
			return count;
		}
};


int main(void)
{
	int n, m;
	int nQueries;

	std::cin >> n >> m;

	Graph g(n, m);;
	g.read_graph();
	//g.print();

	std::cin >> nQueries;
	while (nQueries--) {
		int src, dist;
		std::cin >> src >> dist;
		std::cout << g.bfs_distances(src, dist) << std::endl;
	}

	return 0;
}


