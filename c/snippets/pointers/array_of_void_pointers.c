#include <stdio.h>

int main(void)
{
	void *a[] = {10, 20, 30};
	struct test_struct {
		int tsi;
		char tsc;
	};

	struct test_struct b = {1, 'A'}, c = {2, 'B'};
	void *d[] = {&b, &c};

	int *e = (int *)1234;

	printf ("%d\n", (int)(long)e); /** so sizeof(long) == sizeof(ptr).. no warnings here */

	printf ("%d %d %d %d\n", sizeof(a)/sizeof(void *), (int)a[0], (int)a[1], (int)a[2]);

	printf ("(%d, %c)\n", ((struct test_struct *)d[1])->tsi,((struct test_struct *)d[1])->tsc);
	return 0;
}
