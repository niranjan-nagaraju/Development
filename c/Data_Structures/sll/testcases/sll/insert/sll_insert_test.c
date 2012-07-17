#include <sll.h>
#include <common.h>

struct test_struct {
	int tsi;
	char tsc;
};

void printStruct (void *object)
{
	struct test_struct *obj = (struct test_struct *) object;
	printf("%d %c", obj->tsi, obj->tsc);
}


int main(void)
{
	sll_t sll;
	int i = 10;
	char c = 'a';

	struct test_struct test_obj = {42, 'Z'};

	sll_init(&sll);

	sll_insert_at_front(&sll, (void *)20);
	sll_insert_at_front(&sll, (void *)10);
	sll_insert_at_front(&sll, (void *)30);

	sll_print(&sll, printAsInt);

	return 0;
}
