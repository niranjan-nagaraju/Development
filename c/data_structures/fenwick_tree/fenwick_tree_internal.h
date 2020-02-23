#ifndef _FENWICK_TREE_INTERNAL_H_
#define _FENWICK_TREE_INTERNAL_H_

#include <common.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

/** Internal Implementation routines of fenwick tree */
static int fenwick_tree_read (fenwick_tree_t *ftree, int idx);
static void fenwick_tree_update (fenwick_tree_t *ftree, int idx, int val);
static int fenwick_tree_actual_frequency(fenwick_tree_t *ftree, int idx);
static int fenwick_tree_construct_from_event_frequencies (fenwick_tree_t *ftree, int ftable[], int ftable_len);

/** Unsafe versions of the read and update functions - These do not validate for corner conditions */
static int _fenwick_tree_read (fenwick_tree_t *ftree, int idx);
static void _fenwick_tree_update (fenwick_tree_t *ftree, int idx, int val);
static int _fenwick_tree_actual_frequency(fenwick_tree_t *ftree, int idx);
#endif
