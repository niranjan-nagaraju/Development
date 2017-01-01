#include <common.h>

int
comparePtrs (void *p, int idx1, void *q, int idx2)
{
	return (p == q);
}

int
compareInts (void *p, int idx1, void *q, int idx2)
{
	int a, b;

	if(idx1 == -1) {
		a = (int)p;
	} else {
		a = *((int *)p + idx1);
	}

	if (idx2 == -1) {
		b = (int)q;
	} else {
		b = *((int *)q + idx2);
	}

	return a-b;
}

/** 
 * Compare two int arrays 
 * p[pi...pi+n] vs q[qi..qi+n]
 * Return TRUE if both are equal, else FALSE
 */ 
boolean
compareIntArrays(void *p, int pi, void *q, int qi, int n)
{
	int *l=p, *m=q;
	int i;

	for(i=0; i<n; i++) {
		if (l[pi+i] != m[qi+i]) {
			return FALSE;
		}
	}

	return TRUE;
}

int
compareChars (void *p, int idx1, void *q, int idx2)
{
	char a, b;

	if(idx1 == -1) {
		a = (char)p;
	} else {
		a = *((char *)p + idx1);
	}

	if (idx2 == -1) {
		b = (char)q;
	} else {
		b = *((char *)q + idx2);
	}

	return a-b;
}


