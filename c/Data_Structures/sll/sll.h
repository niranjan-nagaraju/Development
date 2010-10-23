#ifndef _SLL_CORE_H_
#define _SLL_CORE_H_

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>

#include <common.h>

#ifdef _MULTI_THREADED_
	#define	SLL_LOCK_INIT(sll)	pthread_mutex_init(&(sll->lock), NULL)
	#define SLL_LOCK(sll)		pthread_mutex_lock(&(sll->lock))
	#define SLL_UNLOCK(sll)		pthread_mutex_unlock(&(sll->lock))
#else
	#define	SLL_LOCK_INIT(sll) 
	#define SLL_LOCK(sll)
	#define SLL_UNLOCK(sll)
#endif

typedef struct sll_node {
	void *object;
	struct sll_node *next;
} sll_node;

typedef struct sll_s {
	sll_node *head;
	sll_node *tail;

	int _size;	/** Keep a running total of the number of elements in the SLL */

#ifdef _MULTI_THREADED_
	pthread_mutex_t lock;	/** Mutex lock for threadsafing SLL */
#endif
	void (*destroy)(struct sll_s *this, void (*deallocate)(void *object));

	int (*isThreadSafe) (struct sll_s *this);	/** if _MULTI_THREADED_ is defined, the SLL is threadsafe */

	/** Create a SLL node encapsulation of the "object" to be inserted into the SLL */
	sll_node *(*newNode)(void *object);
	void *(*freeNode)(sll_node *node); /** Free the encapsulated SLL node and return its content */

	sll_node *(*next)(struct sll_s *this, sll_node *curr);	/** Retrieve pointer of node next to 'curr' */
	sll_node *(*prev)(struct sll_s *this, sll_node *curr);	/** Retrieve pointer of node previous to 'curr' */
	void *(*value)(sll_node *node);							/** Retrieve the value encapsulated in the SLL node */

	struct sll_s (*fromArray) (void **objects, int n);	/** Construct an SLL from an array */
	void **(*toArray)(struct sll_s *this, void **objects);		/** Retrieve an array of 'objects' stored in the SLL */

	/** Retrieve the position in the SLL if a matching 'object' is found */
	int (*findPos)(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2));

	/** Retrieve the node in the SLL if a matching 'object' is found */
	sll_node *(*findNode)(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2)); 

	int (*length)(struct sll_s *this);	/** Retrieve the number of nodes */

	void *(*objectAt)(struct sll_s *this, int pos);	/** Retrieve object at position */
	sll_node *(*nodeAt)(struct sll_s *this, int pos);	/** Retrieve node at position */
	void *(*objectAtRev)(struct sll_s *this, int pos);	/** Retrieve object at position from the end */
	sll_node *(*nodeAtRev)(struct sll_s *this, int pos);	/** Retrieve node at position from the end */
	void *(*objectAtFront)(struct sll_s *this);		/** Retrieve object at the beginning of the SLL */
	sll_node *(*nodeAtFront)(struct sll_s *this);	/** Retrieve node at the beginning of the SLL */
	void *(*objectAtEnd)(struct sll_s *this);		/** Retrieve object at the end of the SLL */
	sll_node *(*nodeAtEnd)(struct sll_s *this);		/** Retrieve node at the end of the SLL */

	int (*insertAt)(struct sll_s *this, void *object, int pos);	/** Insert an object at a specified position in the SLL */
	int (*insertNodeAt)(struct sll_s *this, sll_node *node, int pos);	/** Insert an SLL node at a specified position */
	int (*insertAtRev)(struct sll_s *this, void *object, int pos);	/** Insert an object at a specified position from the end */
	int (*insertNodeAtRev)(struct sll_s *this, sll_node *node, int pos);/** Insert an SLL node at a specified position from the end */
	int (*insertAtFront)(struct sll_s *this, void *object);				/** Insert at the beginning of the SLL */
	int (*insertNodeAtFront)(struct sll_s *this, sll_node *node);		/** Insert an SLL node at the beginning */
	int (*insertAtEnd)(struct sll_s *this, void *object);				/** Insert at the end of the SLL */
	int (*insertNodeAtEnd)(struct sll_s *this, sll_node *node);			/** Insert an SLL node at the end */
	int (*insertAfter)(struct sll_s *this, sll_node *node, void *object);	/** Insert an object after the specified 'node' */
	int (*insertNodeAfter)(struct sll_s *this, sll_node *node, sll_node *node_to_insert);	/** Insert an SLL node after the specified 'node' */
	int (*insertBefore)(struct sll_s *this, sll_node *node, void *object);					/** Insert an object before the specified 'node' */
	int (*insertNodeBefore)(struct sll_s *this, sll_node *node, sll_node *node_to_insert);	/** Insert an SLL node before the specified 'node' */

	void *(*removeObject)(struct sll_s *this, void *object);	/** Remove an SLL node matching 'object' in the SLL */
	sll_node *(*removeNode)(struct sll_s *this, sll_node *node);	/** Remove the specified 'node' from the SLL */
	void *(*removeAt)(struct sll_s *this, int pos);				/** Remove an object at position and return its content */
	sll_node *(*removeNodeAt)(struct sll_s *this, int pos);		/** Remove an SLL node at position and return it */
	void *(*removeAtRev)(struct sll_s *this, int pos);				/** Remove an object at position from the end and return its content */
	sll_node *(*removeNodeAtRev)(struct sll_s *this, int pos);		/** Remove an SLL node at position from the end and return it */
	void *(*removeAfter)(struct sll_s *this, sll_node *node);	/** Remove an object following the spcified 'node' and return its content */
	sll_node *(*removeNodeAfter)(struct sll_s *this, sll_node *node);	/** Remove an SLL node following the specified 'node' and return it */
	void *(*removeBefore)(struct sll_s *this, sll_node *node);			/** Remove an object preceeding the specified 'node' and return its content */
	sll_node *(*removeNodeBefore)(struct sll_s *this, sll_node *node);	/** Remove an SLL node preceeding the specified 'node' and return it */
	void *(*removeFirst)(struct sll_s *this);			/** Remove first object and return its content */
	sll_node *(*removeFirstNode)(struct sll_s *this);	/** Remove first SLL node and return it */
	void *(*removeLast)(struct sll_s *this);			/** Remove last object and return its content */
	sll_node *(*removeLastNode)(struct sll_s *this);	/** Remove last SLL node and return it */
	
	void (*print)(struct sll_s *this, void (*printfn)(void *object));	/** Print the contents of the SLL */
	void (*printR)(struct sll_s *this, void (*printfn)(void *object));	/** Print the contents of the SLL, recursive version */
	void (*printRev)(struct sll_s *this, void (*printfn)(void *object));	/** Print the contents of the SLL in reverse order */
	void (*printRevR)(struct sll_s *this, void (*printfn)(void *object));	/** Print the contents of the SLL in reverse order, recursive version */

	void (*sort)(struct sll_s *this, int (*compare)(void *obj1, void *obj2));	/** Sort the SLL based on the compare function */
	void (*place)(struct sll_s *this, void *object, int (*compare)(void *obj1, void *obj2)); /** Place the 'object' s.t. SLL nodes are always are sorted */
	void (*placeNode)(struct sll_s *this, sll_node *node, int (*compare)(void *obj1, void *obj2)); /** Place 'node' s.t. SLL nodes are always are sorted */
	int (*isSorted) (struct sll_s *this, int (*compare)(void *obj1, void *obj2));	/** Check if the SLL is sorted */

	void (*reverse)(struct sll_s *this);		/** Reverse the SLL */
	void (*reverseR)(struct sll_s *this);		/** Reverse the SLL, recursive version */
	void (*reverseN)(struct sll_s *this, int n);		/** Reverse first 'n' nodes in the SLL */
	void (*reverseNR)(struct sll_s *this, int n);		/** Reverse first 'n' nodes in the SLL, recursive version */
	void (*reverseNRev)(struct sll_s *this, int n);		/** Reverse last 'n' nodes in the SLL */
	void (*reverseNRevR)(struct sll_s *this, int n);		/** Reverse last 'n' nodes in the SLL, recursive version */

	void (*shiftLeft) (struct sll_s *this, int n);	/** Left shift the SLL by n */
	void (*shiftLeftR) (struct sll_s *this, int n);	/** Left shift the SLL by n, recursive version  */
	void (*shiftRight) (struct sll_s *this, int n);	/** Right shift the SLL by n */
	void (*shiftRightR) (struct sll_s *this, int n); /** Right shift the SLL by n, recursive version */
	void (*rotateLeft) (struct sll_s *this, int n);	/** Left rotate the SLL by n */
	void (*rotateLeftR) (struct sll_s *this, int n); /** Left rotate the SLL by n, recursive version */
	void (*rotateRight) (struct sll_s *this, int n); /** Right rotate the SLL by n */
	void (*rotateRightR) (struct sll_s *this, int n); /** Right rotate the SLL by n, recursive version */

	/** Set operations */
	struct sll_s *(*set_union) (struct sll_s *this, struct sll_s *sll2);	/** Set union of SLL and SLL2 */
	struct sll_s *(*set_intersection) (struct sll_s *this, struct sll_s *sll2);	/** Set Intersection of SLL and SLL2 */
	struct sll_s *(*set_plus) (struct sll_s *this, struct sll_s *sll2);	/** Set Addition of SLL and SLL2 */
	struct sll_s *(*set_minus) (struct sll_s *this, struct sll_s *sll2);	/** Set Minus of SLL and SLL2 */

	int (*hasLoop)(struct sll_s *this);			/** Check if the SLL has a loop */
	int (*hasLoopR)(struct sll_s *this);		/** check if the SLL has a loop, recursive version */
	void (*unLoop) (struct sll_s *this);		/** Unroll the loop and make the SLL linear */

	int (*hasJoin)(struct sll_s *this, struct sll_s *sll2);			/** Check if two SLLs are merged at a joint */
	sll_node *(*nodeOfJoin)(struct sll_s *this, struct sll_s *sll2);	/** Return the node of join if the two SLLs are merged */
	struct sll_s *(*commonNodes)(struct sll_s *this, struct sll_s *sll2);	/** Return the common nodes of the merged SLLs */
	
} sll_t;

/** Core */
void init_sll(struct sll_s *this);	/** Initialize new SLL */

void destroy_sll(struct sll_s *this, void (*deallocate)(void *object)); /** Completely destroy the SLL, use deallocate to manually manage memory */

/** -- BEGIN -- Member functions */
sll_node *newNode_sll(void *object);
void *freeNode_sll(sll_node *node);

int isThreadSafe_sll(struct sll_s *this);

sll_node *next_sll(struct sll_s *this, sll_node *curr);
sll_node *prev_sll(struct sll_s *this, sll_node *curr);

void *value_sll(sll_node *node);

struct sll_s fromArray_sll (void **objects, int n);
void **toArray_sll(struct sll_s *this, void **objects);

int findPos_sll(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2));
sll_node *findNode_sll(struct sll_s *this, void *object, int (*isEqual)(void *obj1, void *obj2));

int length_sll(struct sll_s *this);

void *objectAt_sll(struct sll_s *this, int pos);
sll_node *nodeAt_sll(struct sll_s *this, int pos);
void *objectAtRev_sll(struct sll_s *this, int pos);
sll_node *nodeAtRev_sll(struct sll_s *this, int pos);
void *objectAtFront_sll(struct sll_s *this);
sll_node *nodeAtFront_sll(struct sll_s *this);
void *objectAtEnd_sll(struct sll_s *this);
sll_node *nodeAtEnd_sll(struct sll_s *this);

/** Core */

/** Insert */
int insertAt_sll(struct sll_s *this, void *object, int pos);
int insertNodeAt_sll(struct sll_s *this, sll_node *node, int pos);
int insertAtRev_sll(struct sll_s *this, void *object, int pos);
int insertNodeAtRev_sll(struct sll_s *this, sll_node *node, int pos);
int insertAtFront_sll(struct sll_s *this, void *object);
int insertNodeAtFront_sll(struct sll_s *this, sll_node *node);
int insertAtEnd_sll(struct sll_s *this, void *object);
int insertNodeAtEnd_sll(struct sll_s *this, sll_node *node);
int insertAfter_sll(struct sll_s *this, sll_node *node, void *object);
int insertNodeAfter_sll(struct sll_s *this, sll_node *node, sll_node *node_to_insert);
int insertBefore_sll(struct sll_s *this, sll_node *node, void *object);
int insertNodeBefore_sll(struct sll_s *this, sll_node *node, sll_node *node_to_insert);
/** Insert */

/** Remove */
void *removeObject_sll(struct sll_s *this, void *object);
sll_node *removeNode_sll(struct sll_s *this, sll_node *node);
void *removeAt_sll(struct sll_s *this, int pos);
sll_node *removeNodeAt_sll(struct sll_s *this, int pos);
void *removeAtRev_sll(struct sll_s *this, int pos);	
sll_node *removeNodeAtRev_sll(struct sll_s *this, int pos);
void *removeAfter_sll(struct sll_s *this, sll_node *node);
sll_node *removeNodeAfter_sll(struct sll_s *this, sll_node *node);
void *removeBefore_sll(struct sll_s *this, sll_node *node);
sll_node *removeNodeBefore_sll(struct sll_s *this, sll_node *node);
void *removeFirst_sll(struct sll_s *this);
sll_node *removeFirstNode_sll(struct sll_s *this);
void *removeLast_sll(struct sll_s *this);
sll_node *removeLastNode_sll(struct sll_s *this);
/** Remove */

/** Display */
void print_sll(struct sll_s *this, void (*printfn)(void *object));
void printR_sll(struct sll_s *this, void (*printfn)(void *object));
void printRev_sll(struct sll_s *this, void (*printfn)(void *object));
void printRevR_sll(struct sll_s *this, void (*printfn)(void *object));
/** Display */

/** Sort */
void sort_sll(struct sll_s *this, int (*compare)(void *obj1, void *obj2));
void place_sll(struct sll_s *this, void *object, int (*compare)(void *obj1, void *obj2));
void placeNode_sll(struct sll_s *this, sll_node *node, int (*compare)(void *obj1, void *obj2));
int isSorted_sll(struct sll_s *this, int (*compare)(void *obj1, void *obj2));
/** Sort */

/** Reverse */
void reverse_sll(struct sll_s *this);
void reverseR_sll(struct sll_s *this);
void reverseN_sll(struct sll_s *this, int n);
void reverseNR_sll(struct sll_s *this, int n);
void reverseNRev_sll(struct sll_s *this, int n);
void reverseNRevR_sll(struct sll_s *this, int n);
/** Reverse */

/** Shift/Rotate */
void shiftLeft_sll (struct sll_s *this, int n);
void shiftLeftR_sll (struct sll_s *this, int n);
void shiftRight_sll (struct sll_s *this, int n);
void shiftRightR_sll (struct sll_s *this, int n);
void rotateLeft_sll (struct sll_s *this, int n);
void rotateLeftR_sll (struct sll_s *this, int n);
void rotateRight_sll (struct sll_s *this, int n);
void rotateRightR_sll (struct sll_s *this, int n);
/** Shift/Rotate */

/** Set operations */
struct sll_s *set_union_sll (struct sll_s *this, struct sll_s *sll2);
struct sll_s *set_intersection_sll (struct sll_s *this, struct sll_s *sll2);
struct sll_s *set_plus_sll (struct sll_s *this, struct sll_s *sll2);
struct sll_s *set_minus_sll (struct sll_s *this, struct sll_s *sll2);
/** Set operations */

/** Loop */
int hasLoop_sll(struct sll_s *this);
int hasLoopR_sll(struct sll_s *this);
void unLoop_sll (struct sll_s *this);
/** Loop */

/** Join */
int hasJoin_sll(struct sll_s *this, struct sll_s *sll2);
sll_node *nodeOfJoin_sll(struct sll_s *this, struct sll_s *sll2);
struct sll_s *commonNodes_sll(struct sll_s *this, struct sll_s *sll2);
/** Join */

/** -- END -- Member functions */


#endif

