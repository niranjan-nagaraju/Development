#include <sll.h>
#include <sll_internal.h>


/** Return an initialized object so the user can avoid calling
 *  sll_init() himself and instead initialize by copying 
 *  from an already initialized object
 */
sll_t
sll_lib_initialized_object(void)
{
	static int _initialized = 0;

	/** 
	 * Perform initialization only on the first run
	 * Subsequent runs, just run the already initialized object 
	 */
	if (!_initialized) {
		_initialized = 1;
		sll_init(&_sll_lib_initialized_sll_object);
	}

	return _sll_lib_initialized_sll_object;
}

/** Initialize Singly Linked List */
void 
sll_init (sll_t *sll)
{
	if (!sll)
		return;

	sll->head = sll->tail = NULL;
	sll->_size = 0;

	/** Initialize function pointers */

	/** Core functions */
	sll->length = sll_length;
	sll->destroy = sll_destroy;

	/** Insert operations */
	sll->insert_at_front = sll_insert_at_front;
	sll->insert_at_end = sll_insert_at_end;
	sll->insert_at_position = sll_insert_at_position;
	sll->insert_after = sll_insert_after;
	sll->insert_node_at_front = sll_insert_node_at_front;
	sll->insert_node_at_end = sll_insert_node_at_end;
	sll->insert_node_at_position = sll_insert_node_at_position;
	sll->insert_after_node = sll_insert_after_node;

	/** Remove operations */
	sll->remove_at_front = sll_remove_at_front;
	sll->remove_at_end = sll_remove_at_end;
	sll->remove_at_position = sll_remove_at_position;
	sll->remove_node_at_front = sll_remove_node_at_front;
	sll->remove_node_at_end = sll_remove_node_at_end;
	sll->remove_node_at_position = sll_remove_node_at_position;

	/** Find operations */
	sll->find_containing_node = sll_find_containing_node;
	sll->find = sll_find;
	sll->find_node = sll_find_node;

	/** Print operations */
	sll->print = sll_print;
}


/** Return size of the SLL */
int 
sll_length (sll_t *sll)
{
	if (!sll)
		return -1;

	return sll->_size;
}


/** Destroy SLL completely */
void
sll_destroy (sll_t *sll, deallocatorfn deallocate)
{
	sll_node_t *tmp, *trav;

	if (!sll)
		return;

	/** We don't use tail and size, might as well reset them right here */
	sll->tail = NULL;
	sll->_size = 0;

	trav = sll->head;
	while ( trav != NULL )
	{
		tmp = trav;
		trav = trav->next;

		/** call user supplied memory deallocator */
		if (deallocate)
			deallocate (tmp->data);
	}

	sll->head = NULL;
}
