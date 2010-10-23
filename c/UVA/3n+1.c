#include <stdio.h>
#include <stdlib.h>

int sequence_3n1(int n)
{
	if ( n == 1 )
		return 1;
	if ( n & 1)
		return sequence_3n1(3*n+1) + 1;
	else
		return sequence_3n1(n/2) + 1;
}

int findMaxCycleCount(int i, int j)
{
	int maxCnt = 0;
	int currCnt;

	while( i <= j ) {
		currCnt = sequence_3n1(i);
		if (currCnt > maxCnt)
			maxCnt = currCnt;
		i++;
	}

	return maxCnt;
}

int main()
{
	int m, n;

	while (scanf("%d %d", &m, &n) == 2) {
		if ( m < n) 
			printf("%d %d %d\n", m, n, findMaxCycleCount(m, n));
		else
			printf("%d %d %d\n", m, n, findMaxCycleCount(n, m));
	}
		
	return 0;
}
