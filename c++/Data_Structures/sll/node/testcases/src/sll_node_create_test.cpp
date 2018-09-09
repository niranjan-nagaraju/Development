#include <sll_node.hpp>
#include <assert.h>

#include <iostream>
using namespace std;

/** Don't worry about freeing just yet :) */

int
main (void)
{
	int i = 10;
	char c = 'a';
	struct test_struct {
		int tsi;
		char tsc;
		float f;
	};
	float f = 4.24;

	struct test_struct test_obj = {42, 'Z', 4.24};
	struct test_struct *tsptr;

	sll_node test_node((void *)i);
	i = (int)(size_t)test_node.get();
	assert(10 == i);

	/**
	test_node = sll_node_create ((void *) c);
	assert('a' == (char)test_node.get());
	*/

	test_node = sll_node_create ((void *) &test_obj);
	tsptr = (struct test_struct *) test_node.get();
	cout << tsptr->tsi <<' ' << tsptr->tsc << endl;
	assert(42 == tsptr->tsi &&  'Z' == tsptr->tsc && tsptr->f == f);

	cout << "SLL Node create tests successful" << endl;

	return 0;
}
