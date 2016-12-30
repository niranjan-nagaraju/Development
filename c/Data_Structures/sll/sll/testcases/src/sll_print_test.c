#include <sll.h>
#include <common.h>

struct test_struct {
	int tsi;
	char tsc;
};

void printStruct (void *object)
{
	struct test_struct *obj = (struct test_struct *) object;
	printf("(%d,%c)", obj->tsi, obj->tsc);
}

int main(void)
{
	sll_t sll = SLL_INITIALIZER;

	struct test_struct test_obj = {42, 'Z'}, test_obj2 = {120, 'a'};
	
	sll_insert_at_front(&sll, (void *)&test_obj);
	sll_insert_at_front(&sll, (void *)&test_obj2);

	sll_print(&sll, printStruct);

	return 0;
}
