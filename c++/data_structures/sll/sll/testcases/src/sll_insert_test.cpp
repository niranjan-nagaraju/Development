#include <sll.hpp>
#include <assert.h>
#include <iostream>
using namespace std;


void 
test_int_sll_inserts(void)
{
	sll sll;

	sll.insert_at_front((void *)20); /** [20] */ 
	assert(sll.length() == 1);

	sll.insert_at_front((void *)10); /** [10, 20] */
	assert(sll.length() == 2);

	sll.insert_at_front((void *)30); /** [30, 10, 20] */
	assert(sll.length() == 3);

	sll.insert_at_end((void *) 100); /** [30, 10, 20, 100] */
	assert(sll.length() == 4);
	
	sll.insert_at_position((void *) 120, 2); /** [30, 10, 120, 20, 100] */
	assert(sll.length() == 5);

	sll.insert_at_position((void *) 101, 15);  /** Insert at end  ==> [30, 10, 120, 20, 100, 101] */
	assert(sll.length() == 6); 

	sll.insert_at_position((void *) 122, 5); /** [30, 10, 120, 20, 100, 122, 101] */
	assert(sll.length() == 7); 

	sll.insert_at_end((void *) 140); /** [30, 10, 120, 20, 100, 122, 101, 140] */
	assert(sll.length() == 8); 

	sll.insert_at_position((void *) 150, 8);  /** [30, 10, 120, 20, 100, 122, 101, 140, 150] */
	assert(sll.length() == 9); 

	assert ((int)(size_t)(sll.head().get()) == 30);
	assert ((int)(size_t)(sll.tail().get()) == 150);

	/** Now start with -pos */
	sll.insert_at_position((void *) (-1), -1);  /** [30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length() == 10); 

	sll.insert_at_position((void *) (-10), -10);  /** [-10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length() == 11); 

	sll.insert_at_position((void *) (-15), -15);  /** [-15, -10, 30, 10, 120, 20, 100, 122, 101, 140, -1, 150] */
	assert(sll.length() == 12); 

	assert ((int)(size_t)(sll.head().get()) == (-15));
	assert ((int)(size_t)(sll.tail().get()) == 150);

	//verify_list_against_sll (&sll, (void **)test_list, sizeof(test_list)/sizeof(void *), compareInts);

	/** Test delete too, while at it */
	//sll_delete(&sll, NULL);

	//assert(sll.head == NULL);
	//assert(sll.tail == NULL);
	//assert(sll._size == 0);
}

/**
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

	sll_insert_at_front (&sll, (void *)&a);
	sll_insert_at_end (&sll, (void *) &c);
	sll_insert_at_position (&sll, (void *)&b, 1);

	assert (compareStruct (sll.head->data, -1, &a, -1) == 0);
	assert (compareStruct (sll.tail->data, -1, &c, -1) == 0);

	assert (sll_length(&sll) == 3);
	verify_list_against_sll (&sll, (void **)test_list, sizeof(test_list)/sizeof(void *), compareStruct);

	sll_insert_after (&sll, (void *)&d, (void *)'C', compareCharKey);
	assert (sll_length(&sll) == 4);
	assert (compareStruct (sll.head->data, -1, &a, -1) == 0);
	assert (compareStruct (sll.tail->data, -1, &d, -1) == 0);
}
*/

int main(void)
{
	test_int_sll_inserts();
	//test_struct_sll_inserts();

	printf("SLL Insert tests successful\n");
	return 0;
}
