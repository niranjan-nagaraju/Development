#include <common.h>

#define TEST_SWAP_INT
#define TEST_SWAP_PTR

int main(void)
{
#ifdef TEST_SWAP_INT
	{
		int a = 10;
		int b = -20;

		swapInt (&a, &b);

		printf("SwapInt: %d %d\n", a, b);
	}
#endif

#ifdef TEST_SWAP_PTR
	{
		int a = 10;
		int b = -20;

		int *pa = &a;
		int *pb = &b;

		swapPtr ((void **)&pa, (void **)&pb);
		printf("SwapPtr: %d %d\n", *pa, *pb);
	}
#endif

	return 0;
}
