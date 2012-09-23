#include <sll_test_common.h>

int main(void)
{
	sll_t sll;

	sll_node_t *node;
	int *test_list[] = {-15, -10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150};

	sll_init(&sll);

	assert ((sll.head) == NULL);
	assert ((sll.tail) == NULL);

	sll_insert_at_front(&sll, (void *)20); /** [20] */ 
	assert(sll_length(&sll) == 1);

	sll_insert_at_front(&sll, (void *)10); /** [10, 20] */
	assert(sll_length(&sll) == 2);

	sll_insert_at_front(&sll, (void *)30); /** [30, 10, 20] */
	assert(sll_length(&sll) == 3);

	sll_insert_at_end(&sll, (void *) 100); /** [30, 10, 20, 100] */
	assert(sll_length(&sll) == 4);
	
	sll_insert_at_position(&sll, (void *) 120, 2); /** [30, 10, 120, 20, 100] */
	assert(sll_length(&sll) == 5);

	sll_insert_at_position(&sll, (void *) 101, 15);  /** Insert at end  ==> [30, 10, 120, 20, 100, 101] */
	assert(sll_length(&sll) == 6); 

	sll_insert_at_position(&sll, (void *) 122, 5); /** [30, 10, 120, 20, 100, 122, 101] */
	assert(sll_length(&sll) == 7); 

	sll_insert_at_end(&sll, (void *) 140); /** [30, 10, 120, 20, 100, 122, 101, 140] */
	assert(sll_length(&sll) == 8); 

	sll_insert_at_position(&sll, (void *) 150, 8);  /** [30, 10, 120, 20, 100, 122, 101, 140, 150] */
	assert(sll_length(&sll) == 9); 

	assert ((int)(sll.head->data) == 30);
	assert ((int)(sll.tail->data) == 150);

	/** Now start with -pos */

	sll_insert_at_position(&sll, (void *) (-1), -1);  /** [30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll_length(&sll) == 10); 

	sll_insert_at_position(&sll, (void *) (-10), -10);  /** [-10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll_length(&sll) == 11); 

	sll_insert_at_position(&sll, (void *) (-15), -15);  /** [-15, -10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll_length(&sll) == 12); 

	assert ((int)(sll.head->data) == (-15));
	assert ((int)(sll.tail->data) == 150);

	verify_list_against_sll (&sll, (void **)test_list, sizeof(test_list)/sizeof(void *), compareInts);

	/** Test destory too, while at it */
	sll_destroy(&sll, NULL);

	assert(sll.head == NULL);
	assert(sll.tail == NULL);
	assert(sll._size == 0);

	printf("SLL Insert tests successful\n");

	return 0;
}
