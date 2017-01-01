#ifndef _SLL_H_
#define _SLL_H_

#include <sll_node.h>
#include <common.h>
#include <errno.h>

typedef struct sll_s sll_t;

struct sll_s {
	sll_node_t *head;
	sll_node_t *tail;
	int _size;
};

/** The only public interface that can be called outside of sll object */
void sll_init (sll_t *sll);

sll_t sll_lib_initialized_object (void);

/** An initialized SLL object so the user doesn't have to call sll_init() */
sll_t _sll_lib_initialized_sll_object;

/** Although, this trick won't work on global SLL objects
 *  Wonder how PTHREAD_MUTEX_INITIALIZER works
 *
 *  I could technically in-line macro-define the whole sll_init() here
 *  But everytime sll_init changes, this has to be updated
 *  Also, the private implementation functions will have to be exported here
 *  Not worth the effort :)
 */
#define SLL_INITIALIZER sll_lib_initialized_object();

/** Core operations */
int sll_length (sll_t *sll);

/** Insert operations */
int sll_insert_at_front (sll_t *sll, void *data);
int sll_insert_at_end (sll_t *sll, void *data);
int sll_insert_at_position (sll_t *sll, void *data, int pos);
int sll_insert_after (sll_t *sll, void *data, void *key, comparefn compare);

/** 
 * Insert node operations
 *   separated so that an user of SLL doesnt need to be aware of nodes,
 *   and merely use an SLL as an abstract list storage.
 *
 * Node operations need a data encapsulated node and often times don't 
 * check for various corner conditions as it is assumed the caller will perform such checks.
 */
int sll_insert_node_at_front (sll_t *sll, sll_node_t *node);
int sll_insert_node_at_end (sll_t *sll, sll_node_t *node);
int sll_insert_node_at_position (sll_t *sll, sll_node_t *node, int pos);
void sll_insert_after_node (sll_t *sll, sll_node_t *node, sll_node_t *new_node);

/** Remove operations */
void *sll_remove_at_front (sll_t *sll);
void *sll_remove_at_end (sll_t *sll);
void *sll_remove_at_position (sll_t *sll, int pos);

sll_node_t *sll_remove_node_at_front (sll_t *sll);
sll_node_t *sll_remove_node_at_end (sll_t *sll);
sll_node_t *sll_remove_node_at_position (sll_t *sll, int pos);

/** Find operations */
sll_node_t *sll_find_containing_node (sll_t *sll, void *key, comparefn compare);
void *sll_find (sll_t *sll, void *key, comparefn compare);
boolean sll_find_node (sll_t *sll, sll_node_t *node);

/** Print operations */
void sll_print (sll_t *sll, void(*printfn)(void *));

/** Deallocate operations */
void sll_delete (sll_t *sll, deallocatorfn deallocate);
void sll_destroy (sll_t *sll, deallocatorfn deallocate);

#endif // _SLL_H_
