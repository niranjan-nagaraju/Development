#ifndef _SLL_H_
#define _SLL_H_

#include <sll_node.h>
#include <common.h>

typedef struct sll_s sll_t;

struct sll_s {
	sll_node_t *head;
	sll_node_t *tail;
	int _size;

	/** SLL member functions */

	/** Core functions */
	int (*length)(sll_t *sll);
	void (*destroy)(sll_t *sll, deallocatorfn deallocate);

	/** Insert functions */
	int (*insert_at_front) (sll_t *sll, void *data);
	int (*insert_at_end) (sll_t *sll, void *data);
	int (*insert_at_position) (sll_t *sll, void *data, int pos);
	int (*insert_after) (sll_t *sll, void *data, void *key, comparefn compare);
	int (*insert_node_at_front) (sll_t *sll, sll_node_t *node);
	int (*insert_node_at_end) (sll_t *sll, sll_node_t *node);
	int (*insert_node_at_position) (sll_t *sll, sll_node_t *node, int pos);
	void (*insert_after_node) (sll_t *sll, sll_node_t *node, sll_node_t *new_node);

	/** Remove functions */
	void *(*remove_at_front) (sll_t *sll);
	void *(*remove_at_end) (sll_t *sll);
	void *(*remove_at_position) (sll_t *sll, int pos);
	sll_node_t *(*remove_node_at_front) (sll_t *sll);
	sll_node_t *(*remove_node_at_end) (sll_t *sll);
	sll_node_t *(*remove_node_at_position) (sll_t *sll, int pos);

	/** Find operations */
	sll_node_t *(*find_containing_node) (sll_t *sll, void *key, comparefn compare);
	void *(*find) (sll_t *sll, void *key, comparefn compare);
	boolean (*find_node) (sll_t *sll, sll_node_t *node);

	/** Print operations */
	void (*print) (sll_t *sll, void(*printfn)(void *));
};

/** The only public interface that can be called outside of sll object */
void sll_init (sll_t *sll);

#endif // _SLL_H_
