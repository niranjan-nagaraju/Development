#include <sll.h>
#include <assert.h>
#include <stdio.h>

//sll_t global_sll = SLL_INITIALIZER; // This won't work :(

/** Test if an SLL is properly initialized */
void test_initialization(sll_t *sll)
{
	assert(sll->head == NULL);
	assert(sll->tail == NULL);
	assert(sll->_size == 0);

	assert(sll_length(sll) == 0);
}

/** Initialized explicitly through sll_init() calls */
void test_basic_init (void)
{
	sll_t sll;

	sll_init(&sll);
	
	test_initialization(&sll);
}

/** Copy from an initialized object */
void test_lib_initialized_sll (void)
{
	sll_t sll = SLL_INITIALIZER;

	test_initialization(&sll);
}

int main(void)
{
	test_basic_init();
	test_lib_initialized_sll();

	printf("SLL Initialization tests successful\n");
}
