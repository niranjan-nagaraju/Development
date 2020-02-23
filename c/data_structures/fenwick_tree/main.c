#include <fenwick_tree.h>
#include <stdio.h>

/**
 * From 
 * http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=binaryIndexedTrees
 *
 *		|	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16
 *	----+-------------------------------------------------------------------
 * f	|	1	0	2	1	1	3	0	4	2	5	2	2	3	1	0	2
 * c	|	1	1	3	4	5	8	8	12	14	19	21	23	26	27	27	29
 * tree	|	1	1	2	4	1	4	0	12	2	7	2	11	3	4	0	29
 *	----+-------------------------------------------------------------------
 */

int main(void)
{
	fenwick_tree_t ftree;
	int f[] = {0, 1, 0, 2, 1, 1, 3, 0, 4, 2, 5, 2, 2, 3, 1, 0, 2};

	fenwick_tree_init (&ftree);

	ftree.construct(&ftree, f, 17);

	printf("\nCumulative Frequency at 13 = %d\n", ftree.read(&ftree, 13));
	printf("Cumulative Frequency at 12 = %d\n", ftree.read(&ftree, 12));
	printf("Cumulative Frequency at 10 = %d\n", ftree.read(&ftree, 10));

	printf("Frequency at 13 = %d\n", ftree.frequency(&ftree, 13));
	printf("Frequency at 12 = %d\n", ftree.frequency(&ftree, 12));
	printf("Frequency at 10 = %d\n", ftree.frequency(&ftree, 10));

	return 0;
}
