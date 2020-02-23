/**
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
*/

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

		/** return the minumum distance from src -> dst */
		int bfs_mindistance(int src, int dst) {
			std::queue <std::pair<int, int> > q;
			std::vector<bool> visited(this->nVertices, false);

			visited[src] = true;
			q.push(std::make_pair(src, 0));
			while (!q.empty()) {
				std::pair<int, int> p = q.front();
				q.pop();
				int curr = p.first, dist = p.second;

				if (curr == dst) // Found destination, return distance from src
					return dist;

				for (int v : this->adjlist[curr]) {
					if (!visited[v]) {
						visited[v] = true;
						q.push(std::make_pair(v, dist+1));
					}
				}

			}

			// Completed BFS for all reachable nodes,
			// dst is not reachable from src
			return 0;
		}
};



int main(void)
{
	int n, m;
	int tc;

	std::cin >> tc;

	while (tc--) {
		int n, m;
		std::cin >> n >> m;

		Graph g(n, m);;
		g.read_graph();
		//g.print();
		std::cout << g.bfs_mindistance(1, n) << std::endl;
	}

	return 0;
}

