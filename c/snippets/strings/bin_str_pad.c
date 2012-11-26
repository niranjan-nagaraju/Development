#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Pad to a nibble */

void
bin_str_pad(char *bin_str, int blen, char *bin_str_padded)
{
	int num_zeroes = 4-blen % 4; 
	char zero_bytes[ ] = {'0','0','0'};

	memcpy(bin_str_padded, zero_bytes, num_zeroes);
	memcpy(bin_str_padded+num_zeroes, bin_str, blen+1);
}

int main(void)
{
	char bin_str[] = "110010011";
	char bin_padded[sizeof(bin_str) + 3];
		
	bin_str_pad(bin_str, sizeof(bin_str)-1, bin_padded);
	printf("padded %s = %s\n", bin_str, bin_padded);

	bin_str_pad("101", 3, bin_padded);
	printf("padded %s = %s\n", "101", bin_padded);

	bin_str_pad("11", 2, bin_padded);
	printf("padded %s = %s\n", "11", bin_padded);

	return  0;
}

