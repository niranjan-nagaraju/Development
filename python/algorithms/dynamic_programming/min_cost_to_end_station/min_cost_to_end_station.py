'''
Find the minimum cost to reach destination using a train
There are N stations on route of a train. The train goes from station 0 to N-1. The ticket cost for all pair of stations (i, j) is given where j is greater than i. Find the minimum cost to reach the destination.
Consider the following example:

Input: 
cost[N][N] = { {0, 15, 80, 90},
			  {INF, 0, 40, 50},
			  {INF, INF, 0, 70},
			  {INF, INF, INF, 0}
			 };
There are 4 stations and cost[i][j] indicates cost to reach j 
from i. The entries where j < i are meaningless.

Output:
The minimum cost is 65
The minimum cost can be obtained by first going to station 1 
from 0. Then from station 1 to station 3.
'''


'''
Solution outline: (inefficient)
	minCost(0, n-1) = min{ cost[0][n-1],
						   cost[0][1] + minCost(1, n-1),
						   minCost(1,2) + minCost(2, n-1),
						   . . .
						   minCost(1, n-2) + cost[n-2][n-1]
						  }
'''

def minCost(s, d, cost):
	if (s == d) or (s == d-1):
		return cost[s][d]

	minimum_cost = cost[s][d]
	for i in xrange(s+1, d):
		current_cost = minCost(s, i, cost) + minCost(i, d, cost)
		if current_cost < minimum_cost:
			minimum_cost = current_cost

	return minimum_cost


'''
Use the same recurrence relation, but memoize intermediate results
so they don't get recomputed needlessly
'''
def minCost_memoized(s, d, cost):
	def _mincost_memoized(s, d):
		if memoized_cache.get((s, d)) is not None:
			return memoized_cache[(s,d)]

		if (s == d) or (s == d-1):
			memoized_cache[(s,d)] = cost[s][d]
			return cost[s][d]

		minimum_cost = cost[s][d]
		for i in xrange(s+1, d):
			current_cost = _mincost_memoized(s, i) + _mincost_memoized(i, d)
			if current_cost < minimum_cost:
				minimum_cost = current_cost

		memoized_cache[(s,d)] = minimum_cost
		return minimum_cost

	memoized_cache = {}
	return _mincost_memoized(s, d)



if __name__ == '__main__':
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]
	assert  minCost(0, 3, cost) == 65
	assert  minCost_memoized(0, 3, cost) == 65
