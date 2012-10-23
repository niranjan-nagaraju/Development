#include <fenwick_tree.h>
#include <fenwick_tree_internal.h>


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
	ftree->frequency = fenwick_tree_actual_frequency;
	ftree->construct = fenwick_tree_construct_from_event_frequencies;
}


/** Read cumulative frequency for event at index idx */
static int 
_fenwick_tree_read (fenwick_tree_t *ftree, int idx)
{
	int sum = 0;

	while (idx > 0) {
		sum += ftree->_tree[idx];
		idx -= (idx & -idx);
	}

	return sum;
}

/** Safer read routine */ 
static int 
fenwick_tree_read (fenwick_tree_t *ftree, int idx)
{
	if (!ftree || !ftree->_tree || idx >= ftree->_size)
		return -1;

	return _fenwick_tree_read(ftree, idx);
}

/** Update cumulative frequency for event at index idx */
static void 
_fenwick_tree_update (fenwick_tree_t *ftree, int idx, int val)
{

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


/** Safer update routine */ 
static void 
fenwick_tree_update (fenwick_tree_t *ftree, int idx, int val)
{
	if (!ftree || !ftree->_tree || idx >= ftree->_size)
		return;

	_fenwick_tree_update(ftree, idx, val);
}



/** 
 * Find actual frequency of event 'idx'
 *  = cumulative_freq(idx) - cumulative_freq(idx-1)
 *  = read(idx) - read(idx-1)
 *
 *  if read(idx) and read(idx-1) take a path to root that has common indexes,
 *    We need only 'read' once
 *
 *  If idx is x, idx-1 is y 
 *  let y = a0b` (b` is all 1s)
 *      x = a1b (b is all 0s)
 *      since we 'read' by removing 1s from right
 *      we need z = a0b
 *      one iteration of
 *      idx - (idx & -idx) => z (note z is immediately next to x in the tree)
 *  
 *  so z is where the paths meet
 *  ergo cumulative_freq(z) cancels out in read(x) - read(y)
 *
 *  read(x) = ftree[idx] + ftree[z] + ... + ftree[0]
 *  read(y) = ftree[idx-1] + .... + ftree[z] + ... + ftree[0] 
 *  So ftree[idx] - ( ftree[idx-1] - ... - ftree[z]) = actual frequency
 *
 *  e.g.
 *  f(12) = read(12)- read(11)
 *  however read(12) = tree[12] + tree[8]
 *			read(11) = tree[11] + tree[10] + tree[8]
 *
 *	so frequency(12) = tree[12] - tree[11] - tree[10]
 */      
static int
_fenwick_tree_actual_frequency(fenwick_tree_t *ftree, int idx)
{
	int sum, z;

	/** Fails at 0, return 0, as 0 is not a valid event */
	if (idx <= 0)
		return 0;

	/** Find z */
	z = idx - (idx & -idx);

	/** start at x, next stop is z is anyways.. so stop right here */
	sum = ftree->_tree[idx];

	/** start finding cumulative_freq(y) until z */
	idx --;

	/** Traverse fenwick tree for y until it hits z */
	while (idx != z) {
		sum -= ftree->_tree[idx];
		idx -= (idx & -idx);
	}

	return sum;
}


/** Safer actual frequency routine */ 
static int 
fenwick_tree_actual_frequency(fenwick_tree_t *ftree, int idx)
{
	if (!ftree || !ftree->_tree || idx >= ftree->_size)
		return 0;

	return _fenwick_tree_actual_frequency(ftree, idx);
}


/** Construct a fenwick tree based on a frequency distibution table of events */
static int 
fenwick_tree_construct_from_event_frequencies (fenwick_tree_t *ftree, int ftable[], int ftable_len)
{
	int i = 0;
	int *_tree = NULL;

	_tree = malloc(sizeof(int) * (ftable_len + 1)); /** tree[0] unused */

	if (!ftree)
		return -EINVAL;

	if ( !_tree)
		return -ENOMEM;

	for (i=0; i<ftable_len; i++)
		_tree[i] = 0;

	ftree->_tree = _tree;
	ftree->_size = ftable_len;

	for (i=1; i<ftable_len; i++)
		_fenwick_tree_update(ftree, i, ftable[i]);

	for(i=0; i<ftable_len; i++)
		printf("%d ", _tree[i]);

	return 0;
}


