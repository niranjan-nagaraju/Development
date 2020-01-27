#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


int *nextGreaterElement(int* nums1, int nums1Size, int* nums2, int nums2Size, int* returnSize);

void
test_next_greater_element(int* nums1, int nums1Size, int* nums2, int nums2Size, int *expected_nge)
{
	int i, ret;
	int *nge = nextGreaterElement(nums1, nums1Size, nums2, nums2Size, &ret);

	assert (ret == nums1Size);
	for (i=0; i<ret; i++) {
		assert (nge[i] == expected_nge[i]);
	}

	free(nge);
}

int
main(void)
{
	int a[] = {1,2,3,4};
	int a_[] = {2,4};
	int expected_a[] = {3, -1};

	test_next_greater_element(a_, 2, a, 4, expected_a);

	int b[] = {1,3,4,2};
	int b_[] = {4,1,2};
	int expected_b[] = {-1, 3, -1};
	test_next_greater_element(b_, 3, b, 4, expected_b);

	printf("Tests complete\n");
}
