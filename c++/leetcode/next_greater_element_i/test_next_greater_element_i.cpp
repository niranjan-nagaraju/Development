#include <assert.h>
#include <iostream>
using namespace std;
#include "next_greater_element_i.hpp"

void
//test_next_greater_element(int *n1, int n1sz, int *n2, int n2sz, int *expected_nge, int nge_sz)
test_next_greater_element(vector<int> &a, vector<int> &b, vector<int> &expected_nge)
{
	Solution s;
	int i;

	vector<int>nge = s.nextGreaterElement(a, b);

	assert (nge == expected_nge);
}

int
main(void)
{
	Solution s;
	vector<int> a = {1,2,3,4};
	vector<int> a_{2,4};
	vector<int> expected_a{3, -1};

	test_next_greater_element(a_, a, expected_a);

	vector<int> b{1,3,4,2};
	vector<int> b_{4,1,2};
	vector<int> expected_b{-1, 3, -1};
	test_next_greater_element(b_, b, expected_b);

	cout<<"Tests complete\n";
}
