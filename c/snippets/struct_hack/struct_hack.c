/*
 * A struct-hack is when a structure, struct x, is defined with a 'char
 * array[0]' at the end so sizeof(struct x) returns size excluding the
 * array[0] member, but allocating storage like malloc(sizeof(struct x) +
 * sizeof(char)*100) will mean that array can now be used as if we had
 * defined the struct as 'char arrat[100]'
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <errno.h>

/** 
 * custom SIZEOF to actually measure sizeof struct using pointer arithmetic
 * to ensure struct hacks are not just some compiler trick.
 */
#define SIZEOF(T) \
	(size_t)(((T *)0)+1)

typedef struct hack_s {
	int a;
	int b;
	char c[0];
}hack_t;

typedef struct {
	int a;
	int b;
	char c[];
}hack2_t;


/** Array of size 0 in the middle of the struct */
typedef struct {
	int a;
	char b[0]; /* b[0] can be used as an alias for c */
	int c;
} hack_example2;

#define MAX_STRING_LEN 32

void
print(hack_t *ht)
{
	printf("(%d, %d): %s\n", ht->a, ht->b, ht->c);
}


int
main(void)
{
	hack_t *ht = 0;
	hack2_t *ht2 = 0;

	{
		assert(sizeof(hack_t) == 2 * sizeof(int));
		assert(SIZEOF(hack_t) == 2 * sizeof(int));
		assert(SIZEOF(hack2_t) == 2 * sizeof(int));
		printf("Sizeof struct hack: %lu\n", sizeof(hack_t));

		//c can now be used as c[32]
		ht = malloc(sizeof(hack_t) + sizeof(char) * MAX_STRING_LEN);
		if (!ht)
			return -ENOMEM;

		assert(sizeof(*ht) == 2 * sizeof(int));
		ht->a = 3;
		ht->b = 2;
		strncpy(ht->c, "The quick brown fox jumped over the lazy dog!", MAX_STRING_LEN);
		ht->c[MAX_STRING_LEN - 1] = 0;

		printf("ht: \n");
		print(ht);
	}

	free(ht);

	{
		//c can now be used as c[64]
		ht = malloc(sizeof(hack_t) + sizeof(char) * MAX_STRING_LEN*2);
		if (!ht)
			return -ENOMEM;

		assert(sizeof(*ht) == 2 * sizeof(int));
		ht->a = 3;
		ht->b = 2;
		strncpy(ht->c, "The quick brown fox jumped over the lazy dog!", 2*MAX_STRING_LEN);
		ht->c[2*MAX_STRING_LEN - 1] = 0;

		printf("ht size 64: \n");
		print(ht);
	}

	{
		// ht2 should behave identical to ht
		// i.e. c[0] declaration is the same as c[] declaration
		ht2 = malloc(sizeof(hack2_t) + sizeof(char) * (MAX_STRING_LEN*2));
		if (!ht2)
			return -ENOMEM;

		assert(sizeof(*ht2) == 2 * sizeof(int));
		ht2->a = 6;
		ht2->b = 4;
		strncpy(ht2->c, "The quick brown fox jumped over the lazy dog!", 2*MAX_STRING_LEN);
		ht2->c[2*MAX_STRING_LEN - 1] = 0;

		printf("ht2: \n");
		/** hack_t and hack2_t share the same structure despite declared differently */
		print((hack_t *)ht2); 
	}


	{
		/** zero-size array in the middle of the struct */
		hack_example2 *he = (hack_example2 *)malloc(sizeof(hack_example2));

		assert(sizeof(hack_example2) == 2*sizeof(int));
		assert(SIZEOF(hack_example2) == 2*sizeof(int));

		if (!he)
			return -ENOMEM;
		he->a = 1;
		he->c = 'A';

		/** b[0] can be used as an alias of c */
		assert(he->b[0] == 'A');
		assert(he->c == 65);
	}
	return 0;
}


