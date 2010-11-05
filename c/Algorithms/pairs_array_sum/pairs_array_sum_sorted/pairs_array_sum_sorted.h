#ifndef __PAIRS_SUM__ /** prevent multiple inclusions of this file */
#define __PAIRS_SUM__

#include <stdio.h>

struct pair
{
	int a, b;
};

struct pairs
{
   int num_pairs;   /** number of pairs v found in the array */
   struct pair pairs[100]; /** maximum pairs v handle is 100; only the first num_pairs elements are valid o'course */
};

struct pairs pairs_array_sum(int a[], int n, int sum);

struct pairs pairs_sorted_array_sum(int a[], int n, int sum);
#endif
