#include <sll_node.hpp>
#include <assert.h>

#include<iostream>
using namespace std;

class NodeUT {
	public:
		void test_node_operations(void);
};

/** The destructor will be called at the end of the function for each node */
void 
NodeUT::test_node_operations(void)
{
	int i = 10;
	char c = 'a';
	struct test_struct {
		int tsi;
		char tsc;
		float f;
	};
	float f = 4.24;

	sll_node s1;
	assert(NULL == s1.get());

	struct test_struct test_obj = {42, 'Z', 4.24};
	struct test_struct *tsptr;

	sll_node test_node((void *)i);
	i = (int)(size_t)test_node.get();
	assert(10 == i);

	sll_node test_node2 = sll_node((void *) c);
	assert('a' == (char)(size_t)test_node2.get());

	sll_node test_node3 = sll_node((void *) &test_obj);
	tsptr = (struct test_struct *) test_node3.get();
	assert(42 == tsptr->tsi &&  'Z' == tsptr->tsc && tsptr->f == f);


	/** Test next() and set() */
	sll_node next = test_node3.next(); /** this will be an empty node */
	next.set((void *)20);
	assert(20 == (int)(size_t)next.get());

	/** Test private setNode() */	
	sll_node_t *x = sll_node_create((void *)10);
	sll_node w;
	w.setNode(x);
	
	assert(10 == (int)(size_t)w.get());

	cout << "SLL Node create tests successful" << endl;
}


int
main (void)
{
	NodeUT ut;
	ut.test_node_operations();
	return 0;
}
