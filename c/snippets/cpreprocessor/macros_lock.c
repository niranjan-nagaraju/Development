#include <stdio.h>

#define LOCK_OBJ

#ifdef LOCK_OBJ
#define DO_LOCK(this) printf("Lock this %p\n", &((this)->lock))
#define DO_UNLOCK(this) printf("Unlock this %p\n", &((this)->lock))
#else
#define DO_LOCK(this) 
#define DO_UNLOCK(this)
#endif

struct example {
	char lock[64];
};

int main(void)
{
	struct example obj = {"Lock1"};
	struct example obj2 = {"Lock2"};

	DO_LOCK(&obj);
	DO_LOCK(&obj2);
	DO_UNLOCK(&obj2);
	DO_UNLOCK(&obj);

	return 0;
}
