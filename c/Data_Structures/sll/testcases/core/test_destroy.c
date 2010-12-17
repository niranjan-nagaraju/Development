#include <sll.h>

/** Custom memory manager for SLL2 */
void *
my_allocate (int size)
{
	return malloc(size);
}

void
my_free (void *obj)
{
	free(obj);
	obj = NULL;
}

/** Objectes to be used in SLL2 */
struct sll2_obj
{
	int i, j;
};

struct sll2_obj *
allocate_sll2_obj (int i, int j)
{
	struct sll2_obj *obj = (struct sll2_obj *)my_allocate(sizeof(struct sll2_obj));
	if (obj) {
		obj->i = i;
		obj->j = j;		
	}

	return obj;
}

void 
printsll2_obj(void *ob)
{
	struct sll2_obj *obj = ob;

	printf("(%d %d)", obj->i, obj->j);
}

int 
main(void)
{
	sll_t *sll;	/** SLL1 uses static allocation, but the container itself is managed on heap */ 
	sll_t sll2; /** SLL2 uses dynamic memory allocation using its own memory manager, but the container itself is on the stack */
	int i;

	sll = (sll_t *)malloc(sizeof(sll_t));
	if (!sll) {
		printf("Could not allocate memory!\n");
		exit(1);
	}

	init_sll(sll);
	init_sll(&sll2);

	for (i=0; i<10; i++) {
		sll->insertAtFront(sll, (void *)i);
	}

	printf("Printing SLL\n");
	sll->print(sll, printAsInt);

	for (i=0; i<10; i++) {
		sll2.insertAtFront(&sll2, allocate_sll2_obj(i,(i+1)));	/** use custom allocator */
	}

	printf("Printing SLL2\n");
	sll2.print(&sll2, printsll2_obj);

	destroy_sll(sll, 0);
	destroy_sll(&sll2, my_free);	/** Use custom de-allocator */

	return 0;
}
