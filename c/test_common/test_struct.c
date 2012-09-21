#include <test_struct.h>

/** compare int fields in the struct */
int
compareIntKey (void *obj1, void *obj2)
{
	return (((struct test_struct *)(obj1))->tsi - (int)obj2);
}

/** compare char fields in the struct */
int
compareCharKey (void *obj1, void *obj2)
{
	return (((struct test_struct *)(obj1))->tsc - (int)obj2);
}


