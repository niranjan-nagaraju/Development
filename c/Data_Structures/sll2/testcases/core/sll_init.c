#include <sll.h>
#include <assert.h>

int main(void)
{
	sll_t sll;

	sll_init (&sll);

	printf("Test sll_init()\n");
	assert (sll.head == NULL);
	assert (sll.tail == NULL);
	assert (sll.size == 0);
	printf("Test sll_init() SUCCESS!\n");

	return 0;
}
