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

#include <stdio.h>
#include <stdlib.h>

/** Nodes for adjancency lists and BFS queues */
typedef struct Node_ {
	int vertex;
	int distance;
	struct Node_ *next;
} Node;


/** Linkedlist to be used for BFS queue and adjacency lists for the graph */
typedef struct LinkedList_ {
	Node *head;
	Node *tail;
} LinkedList;


/** Graph: contains number of vertices, edges and the adjacency list */
typedef struct Graph_ {
	int nVertices;
	int nEdges;
	LinkedList* adjlist;
} Graph;


/** Node functions */
Node *
new_node(int v, int d)
{
	Node *n = malloc(sizeof(Node)); 

	n->vertex = v;
	n->distance = d;
	n->next = 0;

	return n;
}
/** Node functions */

/** Linked List functions */
/** Initialize a new list and return it */
LinkedList
new_list(void)
{
	LinkedList ll = {0, 0};
	return ll;
}

/** Push (vertex, distance) at the back of the linked list */
void
push_back(LinkedList *ll, int vertex, int distance)
{
	Node *node = new_node(vertex, distance);

	if (!ll->head) {
		ll->head = ll->tail = node;
		return;
	}

	ll->tail->next = node;
	ll->tail = node;
}


/** Remove a node from the front of the linked list and return it */
Node *
pop_front(LinkedList *ll)
{
	if (!ll->head)
		return 0;

	Node *node = ll->head;
	ll->head = ll->head->next;
	node->next = 0; /** sanitize */

	if (!ll->head)
		ll->tail = ll->head = 0;

	return node;
}

/** Print the linked list */
void
print_list(LinkedList *ll)
{
	Node *node = ll->head;

	while (node) {
		printf("(%d %d) ", node->vertex, node->distance);
		node = node->next;
	}
	printf("\n");
}


/** Deallocate memory allocated to all the nodes in the chain */
void
destroy_list(LinkedList *l)
{
	Node *node = l->head;
	Node *prev;
	while (node) {
		prev = node;
		node = node->next;
		free(prev);
	}

	l->head = l->tail = 0;
}
/** Linked List functions */


/** Graph functions */
/** Initialize a new graph and return it */
Graph
new_graph(int n, int m)
{
	Graph g = {n+1, m, 0}; // 1-indexing, 0 is unused
	int i;

	g.adjlist = (LinkedList *) malloc(sizeof(LinkedList) * g.nVertices);

	for (i=0; i<g.nVertices; i++) {
		g.adjlist[i].head = 0;
		g.adjlist[i].tail = 0;
	}

	return g;
}


/** Read (src, dst) pairs and add edges to the graph */
void
read_graph(Graph *g)
{
	int i;

	for(i=0; i<g->nEdges; i++) {
		int src, dst;
		scanf("%d %d", &src, &dst);

		push_back(&g->adjlist[src], dst, 0);
		push_back(&g->adjlist[dst], src, 0);
	}
}


/** Print a graph */
void
print_graph(Graph *g)
{
	int i;

	printf("Graph: \n");
	for (i=0; i<g->nVertices; i++) {
		printf("%d: ", i);
		print_list(&g->adjlist[i]);
	}
}

/** Deallocate memory allocated to the adjancency list and all the adjacency list chains */
void
destroy_graph(Graph *g)
{
	int i;
	for (i=0; i<g->nVertices; i++) {
		LinkedList *ll = &g->adjlist[i];
		destroy_list(ll);
	}

	free(g->adjlist);
}


/** Return minimum distance from src -> dst */
int
bfs_mindistance(Graph *g, int src, int dst)
{
	LinkedList bfs_q = new_list();
	int visited[g->nVertices];
	int i;

	for (i=0; i<g->nVertices; i++)
		visited[i] = 0;

	push_back(&bfs_q, src, 0);
	visited[src] = 1;

	while (bfs_q.head) {
		Node *node = pop_front(&bfs_q);
		int curr = node->vertex, dist = node->distance;
		free(node);

		// Found destination, return distance from src
		if (curr == dst)
			return dist;

		LinkedList ll = g->adjlist[curr];
		Node *tmp = ll.head;
		while (tmp) {
			if (!visited[tmp->vertex]) {
				visited[tmp->vertex] = 1;
				push_back(&bfs_q, tmp->vertex, dist+1);
			}
			tmp = tmp->next;
		}
	}

	// Completed BFS for all reachable nodes,
	// dst is not reachable from src
	return 0;
}
/** Graph functions */



int
main(void)
{
	Graph g;
	int i;
	int n, m;
	int tc;

	scanf("%d", &tc);
	while (tc--) {
		scanf("%d %d", &n, &m);
		g = new_graph(n, m);
		read_graph(&g);

		//print_graph(&g);
		printf("%d\n", bfs_mindistance(&g, 1, n));
		destroy_graph(&g);
	}

	return 0;
}

