#include <sll.hpp>
#include <assert.h>


/** Test if an SLL is properly initialized */
void test_initialization(sll *slist)
{
    assert(slist->head().get() == NULL); // TODO: Overload == operator to compare against two sll_nodes
    assert(slist->tail().get() == NULL);
    assert(slist->length() == 0);
}


int main(void)
{
	printf("SLL Init tests successful\n");
	return 0;
}
