#ifndef __FENWICK_TREE_H_
#define __FENWICK_TREE_H_

typedef struct fenwick_tree_s fenwick_tree_t;

struct fenwick_tree_s
{
	int *_tree; /** Store fenwick tree */
	int _size;  /** Number of entries in the tree == Number of events */

	/** Read cumulative frequency for event at index idx */
	int (*read) (fenwick_tree_t *, int idx);

	/** Update cumulative frequency for event at index idx */
	void (*update) (fenwick_tree_t *, int idx, int val);

	/** Actual frequency for event at index idx */
	int (*frequency) (fenwick_tree_t *, int idx);

	/** 
	 * Construct a Fenwick tree from a frequency distribution table
	 *  => call update() for frequencies at every event
	 */
	int (*construct) (fenwick_tree_t *, int frequency_table[], int frequency_table_len);
};

void fenwick_tree_init(fenwick_tree_t *ftree);

#endif
