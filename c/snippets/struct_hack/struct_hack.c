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

typedef struct hack_s {
	int a;
	int b;
	char c[0];
}hack_t;

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
	hack_t *ht2 = 0;

	assert(sizeof(hack_t) == 2 * sizeof(int));
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


	//c can now be used as c[64] in ht2
	ht2 = malloc(sizeof(hack_t) + sizeof(char) * (MAX_STRING_LEN*2));
	if (!ht2)
		return -ENOMEM;

	assert(sizeof(*ht2) == 2 * sizeof(int));
	ht2->a = 6;
	ht2->b = 4;
	strncpy(ht2->c, "The quick brown fox jumped over the lazy dog!", 2*MAX_STRING_LEN);
	ht2->c[2*MAX_STRING_LEN - 1] = 0;

	printf("ht2: \n");
	print(ht2);
	return 0;
}


