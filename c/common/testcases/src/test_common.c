#include <common.h>

int main(void)
{
	{
		int a = 10;
		int b = -20;

		swapInt (&a, &b);

		printf("SwapInt: %d %d\n", a, b);
	}

	{
		int a = 10;
		int b = -20;

		int *pa = &a;
		int *pb = &b;

		swapPtr ((void **)&pa, (void **)&pb);
		printf("SwapPtr: %d %d\n", *pa, *pb);
	}

	return 0;
}
