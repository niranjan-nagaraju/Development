#include <fenwick_tree.h>

/** Initialize fenwick tree 
 *   Reset size and tree 
 *   Initialize function pointers
 */
void fenwick_tree_init (fenwick_tree_t *ftree)
{
	if (!ftree)
		return;

	ftree->_tree = NULL;
	ftree->_size = 0;

	ftree->read = fenwick_tree_read;
	ftree->update = fenwick_tree_update;
	ftree->construct = fenwick_tree_construct;
}

/** Read cumulative frequency for event at index idx */
int 
fenwick_tree_read (fenwick_tree_t *ftree, int idx)
{
	int sum = 0;

	if (!ftree || !ftree->_tree || idx >= ftree->_size)
		return -1;

	while (idx > 0) {
		sum += ftree->_tree[idx];
		idx -= (idx & -idx);
	}

	return sum;
}

/** Update cumulative frequency for event at index idx */
void 
fenwick_tree_update (fenwick_tree_t *ftree, int idx, int val)
{
	if (!ftree || !ftree->_tree || idx >= ftree->_size)
		return -1;

	/** FIXME: 
	 *   Technically we should be able to update with
	 *   a frequency at a new event > ftree->_size
	 *   but we'd also have to re-percolate that frequency
	 *   into the fenwick tree..
	 *   For now, once 'constructed' only allow updates within the
	 *   allocated tree space.. not outside
	 */
	while (idx <= ftree->_size) {
		ftree->_tree[idx] += val;
		idx += (idx & -idx);
	}
}


int 
fenwick_tree_construct_from_event_frequencies (fenwick_tree_t *ftree, int ftable[], int ftable_len)
{
	int i = 0;
	int *_tree = NULL;

	_tree = malloc(sizeof(int) * (ftable_len + 1)); /** tree[0] unused */

	if ( !_tree)
		return -ENOMEM;

	for (i=1; i<; i++)
		update(tmp_tree, 17, i, f[i]);

	return 0;
}

int main(void)
{
	printf("%d\n", read(13));

	construct_tree_from_frequencies(f);

	return 0;
}
