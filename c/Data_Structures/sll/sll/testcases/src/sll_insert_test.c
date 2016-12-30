#include <sll_test_common.h>


void 
test_int_sll_inserts(void)
{
	sll_t sll = SLL_INITIALIZER;

	sll_node_t *node;
	int *test_list[] = {-15, -10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150};

	assert ((sll.head) == NULL);
	assert ((sll.tail) == NULL);

	sll.insert_at_front(&sll, (void *)20); /** [20] */ 
	assert(sll.length(&sll) == 1);

	sll.insert_at_front(&sll, (void *)10); /** [10, 20] */
	assert(sll.length(&sll) == 2);

	sll.insert_at_front(&sll, (void *)30); /** [30, 10, 20] */
	assert(sll.length(&sll) == 3);

	sll.insert_at_end(&sll, (void *) 100); /** [30, 10, 20, 100] */
	assert(sll.length(&sll) == 4);
	
	sll.insert_at_position(&sll, (void *) 120, 2); /** [30, 10, 120, 20, 100] */
	assert(sll.length(&sll) == 5);

	sll.insert_at_position(&sll, (void *) 101, 15);  /** Insert at end  ==> [30, 10, 120, 20, 100, 101] */
	assert(sll.length(&sll) == 6); 

	sll.insert_at_position(&sll, (void *) 122, 5); /** [30, 10, 120, 20, 100, 122, 101] */
	assert(sll.length(&sll) == 7); 

	sll.insert_at_end(&sll, (void *) 140); /** [30, 10, 120, 20, 100, 122, 101, 140] */
	assert(sll.length(&sll) == 8); 

	sll.insert_at_position(&sll, (void *) 150, 8);  /** [30, 10, 120, 20, 100, 122, 101, 140, 150] */
	assert(sll.length(&sll) == 9); 

	assert ((int)(sll.head->data) == 30);
	assert ((int)(sll.tail->data) == 150);

	/** Now start with -pos */

	sll.insert_at_position(&sll, (void *) (-1), -1);  /** [30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length(&sll) == 10); 

	sll.insert_at_position(&sll, (void *) (-10), -10);  /** [-10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length(&sll) == 11); 

	sll.insert_at_position(&sll, (void *) (-15), -15);  /** [-15, -10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length(&sll) == 12); 

	assert ((int)(sll.head->data) == (-15));
	assert ((int)(sll.tail->data) == 150);

	verify_list_against_sll (&sll, (void **)test_list, sizeof(test_list)/sizeof(void *), compareInts);

	/** Test destory too, while at it */
	sll.destroy(&sll, NULL);

	assert(sll.head == NULL);
	assert(sll.tail == NULL);
	assert(sll._size == 0);

}

void 
test_struct_sll_inserts(void)
{
	sll_t sll;

	sll_node_t *node;
	struct test_struct a = {1, 'A'}, b = {2, 'B'}, c = {3, 'C'}, *test_list[] = {&a , &b, &c};
	struct test_struct d = {4, 'D'};

	sll_init(&sll);

	assert ((sll.head) == NULL);
	assert ((sll.tail) == NULL);

	sll.insert_at_front (&sll, (void *)&a);
	sll.insert_at_end (&sll, (void *) &c);
	sll.insert_at_position (&sll, (void *)&b, 1);

	assert (compareStruct (sll.head->data, -1, &a, -1) == 0);
	assert (compareStruct (sll.tail->data, -1, &c, -1) == 0);

	assert (sll.length(&sll) == 3);
	verify_list_against_sll (&sll, (void **)test_list, sizeof(test_list)/sizeof(void *), compareStruct);

	sll.insert_after (&sll, (void *)&d, (void *)'C', compareCharKey);
	assert (sll.length(&sll) == 4);
	assert (compareStruct (sll.head->data, -1, &a, -1) == 0);
	assert (compareStruct (sll.tail->data, -1, &d, -1) == 0);
}


int main(void)
{
	test_int_sll_inserts();
	test_struct_sll_inserts();

	printf("SLL Insert tests successful\n");
	return 0;
}
