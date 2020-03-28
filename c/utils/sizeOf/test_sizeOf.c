#include "sizeOf.h"
#include <assert.h>

typedef struct Test_{
	int i;
	char c;
	float f;
} Test;

typedef struct Test2_{
	double d;
	char c1;
	char c2;
	int i;
	float f;
} Test2;


typedef union UTest_{
	char c;
	int i;
	float f;
} UTest;

int
main(void)
{
	assert((int)SIZEOF(int) == sizeof(int));
	assert((int)SIZEOF(double) == sizeof(double));
	assert((int)SIZEOF(Test) == SIZEOF(int)+SIZEOF(int)+SIZEOF(float));
	assert((int)SIZEOF(Test2) == SIZEOF(double)+3*SIZEOF(int)+SIZEOF(float));
	assert((int)SIZEOF(UTest) == SIZEOF(float));

	return 0;
}
