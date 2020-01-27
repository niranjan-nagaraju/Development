#include <assert.h>
#include <iostream>
using namespace std;
#include "next_greater_element_i.hpp"

void
test_next_greater_element(int *n1, int n1sz, int *n2, int n2sz, int *expected_nge, int nge_sz)
{
	Solution s;
	int i;

	vector<int> a(n1, n1+n1sz);
	vector<int> b(n2, n2+n2sz);
	vector<int> e_nge(expected_nge, expected_nge+nge_sz);

	vector<int>nge = s.nextGreaterElement(a, b);

	assert (nge == e_nge);
}

int
main(void)
{
	Solution s;
	int a[] = {1,2,3,4};
	int a_[] = {2,4};
	int expected_a[] = {3, -1};

	test_next_greater_element(a_, 2, a, 4, expected_a, 2);

	int b[] = {1,3,4,2};
	int b_[] = {4,1,2};
	int expected_b[] = {-1, 3, -1};
	test_next_greater_element(b_, 3, b, 4, expected_b, 3);

	cout<<"Tests complete\n";
}
