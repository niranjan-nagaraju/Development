#include <stdarg.h>
#include <assert.h>

/**
 * Return a sum of the numbers in the arguments list
 * First argument contains length of the list, the argument list follows
 *
 * All numbers in the list are assumed to be integers
 */
int
sum(int num_args, ...)
{
	va_list vl;
	int i, sum_value=0;

	va_start(vl, num_args);

	for (i=0; i<num_args; i++) {
		int x = va_arg(vl, int);
		sum_value += x;
	}

	va_end(vl);
	return sum_value;
}

int main(void)
{ 
	/* Considers only first arguments,
	 * since the first argument mentions there are 5 arguments
	 */
	assert( sum(5, 6, 5, 4, 3, 2, 1) == 6+5+4+3+2);
	assert( sum(3, 26, 24, 22) == 72 );
	return 0;
}
