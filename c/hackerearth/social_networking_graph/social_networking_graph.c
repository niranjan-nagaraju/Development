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


int
bfs_distances(Graph *g, int src, int distance)
{
	LinkedList bfs_q = new_list();
	int visited[g->nVertices];
	int i, count=0;

	for (i=0; i<g->nVertices; i++)
		visited[i] = 0;

	push_back(&bfs_q, src, 0);
	visited[src] = 1;

	while (bfs_q.head) {
		Node *node = pop_front(&bfs_q);
		int curr = node->vertex, dist = node->distance;
		free(node);

		/** 
		 * We are done with all nodes at required distance
		  * return nodes counted so far
		  */
		if (dist > distance)
			return count;
		else if (dist == distance) // Count all nodes at distance,
			count += 1;

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

	return count;
}
/** Graph functions */



int
main(void)
{
	Graph g;
	int i;
	int n, m;
	int nQueries;

	scanf("%d %d", &n, &m);
	g = new_graph(n, m);
	read_graph(&g);

	//print_graph(&g);

	scanf("%d", &nQueries);

	for (i=0; i<nQueries; i++) {
		int src, dist;
		scanf("%d %d", &src, &dist);

		printf("%d\n", bfs_distances(&g, src, dist));
	}

	destroy_graph(&g);
	return 0;
}


