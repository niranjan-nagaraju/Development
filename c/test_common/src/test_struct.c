#include <test_struct.h>

/** compare int fields in the struct */
int
compareIntKey (void *obj1, int idx1, void *obj2, int idx2)
{
	return (((struct test_struct *)(obj1))->tsi - (int)obj2);
}

/** compare char fields in the struct */
int
compareCharKey (void *obj1, int idx1, void *obj2, int idx2)
{
	return (((struct test_struct *)(obj1))->tsc - (int)obj2);
}

int
compareStruct (void *obj1, int idx1, void *obj2, int idx2)
{
	return ( (((struct test_struct *)obj1)->tsi - ((struct test_struct *)obj1)->tsi) &&
			 (((struct test_struct *)obj1)->tsc - ((struct test_struct *)obj1)->tsc) );
}
