#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Pad to a nibble */

char *
bin_str_pad(char *bin_str, int blen)
{
	int num_zeroes = 4-blen % 4; 
	char zero_bytes[ ] = {'0','0','0'};

	char *bin_str_padded = malloc(4+blen); /** allocate 4 additional bytes, 3 for maximum padding, 1 for '\0' */

	memcpy(bin_str_padded, zero_bytes, num_zeroes);
	memcpy(bin_str_padded+num_zeroes, bin_str, blen+1);

	return bin_str_padded;
}

int main(void)
{
	char bin_str[] = "110010011";
	char *bin_padded = NULL;
		
	bin_padded = bin_str_pad(bin_str, sizeof(bin_str)-1);
	printf("padded %s = %s\n", bin_str, bin_padded);
	free(bin_padded);

	bin_padded = bin_str_pad("101", 3);
	printf("padded %s = %s\n", "101", bin_padded);
	free(bin_padded);

	bin_padded = bin_str_pad("11", 2);
	printf("padded %s = %s\n", "11", bin_padded);
	free(bin_padded);

	return  0;
}

