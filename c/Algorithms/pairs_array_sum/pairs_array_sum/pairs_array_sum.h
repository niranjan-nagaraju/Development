#ifndef __PAIR_SUM__ /** prevent multiple inclusions of this file */
#define __PAIR_SUM__

#include <stdio.h>

struct pair
{
	int a, b;
};

struct pairs
{
	int num_pairs;
	struct pair pairs[100];
};

struct pairs 
pairs_array_sum(int a[], int n, int sum);

#endif
