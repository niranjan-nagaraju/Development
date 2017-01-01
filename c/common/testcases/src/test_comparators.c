#include <common.h>
#include <assert.h>

int main(void)
{
	{
		int a=1,b=2;
		int la[] = {5,4,3,2,1}, lb[] = {6,5,4,3,2,1};

		assert(compareInts(a, -1, b, -1) < 0);
		assert(compareInts(la, 1, lb, 2) == 0);
		assert(compareInts(la, 0, lb[4], -1) > 0);
	}
	{
		char a='A',b='B';
		char* la = "abcde", *lb = "abcdef";

		assert(compareChars(a, -1, b, -1) < 0);
		assert(compareChars(la, 2, lb, 2) == 0);
		assert(compareChars(la, 4, lb[0], -1) > 0);
	}

	/** Test Int array comparator */
	{
		int a[] = {1,2,3,4,5,6};
		int b[] = {2,3,4,5,6,7,8};

		assert(compareIntArrays(a, 1, b, 0, 5) == TRUE);
		assert(compareIntArrays(a, 0, b, 0, 5) == FALSE);
	}

	return 0;
}
