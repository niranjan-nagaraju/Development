#include <fenwick_tree.h>
#include <stdio.h>

int main(void)
{
	fenwick_tree_t ftree;
	int f[] = {0, 1, 0, 2, 1, 1, 3, 0, 4, 2, 5, 2, 2, 3, 1, 0, 2};

	fenwick_tree_init (&ftree);

	ftree.construct(&ftree, f, 17);

	printf("\n%d\n", ftree.read(&ftree, 13));

	return 0;
}
