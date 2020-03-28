#include "offsetOf.h"
#include <assert.h>
#include <stdio.h>

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
	assert((int)OFFSETOF(Test, i) == 0);
	assert((int)OFFSETOF(Test, c) == sizeof(int));
	assert((int)OFFSETOF(Test, f) == sizeof(int)+sizeof(float));

	assert((int)OFFSETOF(UTest, i) == 0);
	assert((int)OFFSETOF(UTest, c) == 0);
	assert((int)OFFSETOF(UTest, f) == 0);

	assert((int)OFFSETOF(Test2, d) == 0);
	assert((int)OFFSETOF(Test2, c1) == sizeof(double));
	assert((int)OFFSETOF(Test2, c2) == sizeof(double)+1);
	assert((int)OFFSETOF(Test2, f) == sizeof(double)+sizeof(int)+sizeof(float));
	assert((int)OFFSETOF(Test2, i) == sizeof(double)+sizeof(int));

	return 0;
}
