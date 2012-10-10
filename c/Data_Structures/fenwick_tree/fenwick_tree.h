#ifndef __FENWICK_TREE_H_
#define __FENWICK_TREE_H_

typedef struct fenwick_tree_s
{
	int *_tree; /** Store fenwick tree */
	int _size;  /** Number of entries in the tree == Number of events */

	/** Read cumulative frequency for event at index idx */
	int (*read) (struct fenwick_tree_s *, int idx);

	/** Update cumulative frequency for event at index idx */
	void (*update) (struct fenwick_tree_s *, int idx, int val);

	/** 
	 * Construct a Fenwick tree from a frequency distribution table
	 *  => call update() for frequencies at every event
	 */
	int (*construct) (struct fenwick_tree_s *, int frequency_table[], int frequency_table_len);
} fenwick_tree_t;

void fenwick_tree_init(fenwick_tree_t *ftree);

#endif
