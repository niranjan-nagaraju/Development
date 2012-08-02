#include <common.h>

int comparePtrs (void *p, void *q)
{
	return (p == q);
}

int compareInts (void *p, void *q)
{
	int i = (int)p, j = (int)q;

	if (i < j)
		return -1;
	else if (i == j)
		return 0;
	else /** (i > j) - law of trichotomy :P */
		return 1;
}

int compareChars (void *p, void *q)
{
	char i = (char)p, j = (char)q;

	if (i < j)
		return -1;
	else if (i == j)
		return 0;
	else /** (i > j) - law of trichotomy :P */
		return 1;
}

void swapInt(int *p, int *q)
{
	/** NOTE: if the same pointers are passed, the numbers become zero right after the first xor */
	if (p == q)
		return;

#define P *p
#define Q *q
	P = P ^ Q;
	Q = P ^ Q;	/** p ^ q ^ q = p */
	P = P ^ Q;	/** now q = prev value of p, so,  p ^ q ^ p = q */
#undef P
#undef Q
}

void swapChar(char *p, char *q)
{
	/** NOTE: if the same pointers are passed, the values become zero right after the first xor */
	if (p == q)
		return;

#define P *p
#define Q *q
	P = P ^ Q;
	Q = P ^ Q;	/** p ^ q ^ q = p */
	P = P ^ Q;	/** now q = prev value of p, so,  p ^ q ^ p = q */
#undef P
#undef Q
}

void swapPtr(void **p, void **q)
{
	/** NOTE: if the same pointers are passed, the values become zero right after the first xor */
	if (p == q)
		return;

#define P (long)*p
#define Q (long)*q
	*p = (void *)(P ^ Q);
	*q = (void *)(P ^ Q);	/** p ^ q ^ q = p */
	*p = (void *)(P ^ Q);	/** now q = prev value of p, so,  p ^ q ^ p = q */
#undef P
#undef Q
}

void printAsInt(void *object)
{
	printf("%d", (int)object);
}

void printAsUInt(void *object)
{
	printf("%u", (unsigned int)object);
}

void printAsLong(void *object)
{
	printf("%ld", (long)object);
}

void printAsULong(void *object)
{
	printf("%lu", (unsigned long)object);
}

void printAsChar (void *object)
{
	printf("%c", (char)object);
}


void printAsString(void *object)
{
	printf("%s", (char *)object);
}

void printPtr(void *object)
{
	printf("%p", object);
}

void printNL(void)
{
	printf("\n");
}

