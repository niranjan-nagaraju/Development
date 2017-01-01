#include <sll.h>
#include <stdlib.h>

/** 
 * Delete SLL state 
 * Deletes only the internal state of the SLL struct sll_t,
 * not allocation for sll_t itself, Use sll_destroy to do that.
 *
 * Why two separate functions to cleanup?
 *   sll_delete() can be called for stack allocations for sll_t,
 *   sll_destroy() when sll_t* itself is allocated dynamically thereby needing to deallocate
 *   the pointer allocation as well.
 */


/** Unsafe version, doesn't do safety checks */
static void
_sll_delete (sll_t *sll, deallocatorfn deallocate)
{
	sll_node_t *tmp, *trav;

	/** We don't use tail and size, might as well reset them right here */
	sll->tail = NULL;
	sll->_size = 0;

	trav = sll->head;
	while ( trav != NULL )
	{
		tmp = trav;
		trav = trav->next;

		deallocate (tmp->data);
		free(tmp);
	}

	sll->head = NULL;
}

/** Delete and (additionally) destroy based on the flag */
void
_sll_deallocate (sll_t *sll, deallocatorfn deallocate, boolean destroy)
{	
	if (!sll)
		return;

	_sll_delete(sll, deallocate);

	/** Destroy the 'sll' allocation as well if the flag is set */
	if (destroy)
		deallocate(sll);
}


/** Delete SLL internal state */
void
sll_delete (sll_t *sll, deallocatorfn deallocate)
{
	_sll_deallocate(sll, deallocate, FALSE);
}


/** Destroy SLL completely including the allocation for sll_t itself */
void
sll_destroy (sll_t *sll, deallocatorfn deallocate)
{
	_sll_deallocate(sll, deallocate, TRUE);
}

