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
Solution outline: (inefficient), O(2^n)
	minCost(0, n-1) = min{ cost[0][n-1],
						   cost[0][1] + minCost(1, n-1),
						   minCost(0,2) + minCost(2, n-1),
						   . . .
						   minCost(0, n-2) + cost[n-2][n-1]
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
O(n^3) runtime, O(n^2) space
'''
def minCost_memoized(s, d, cost):
	def generate_memoized_table(s, d, memoized_cache):
		if (s == d) or (s == d-1):
			memoized_cache[s][d] = cost[s][d]
			return memoized_cache[s][d]

		minimum_cost = cost[s][d]
		for i in xrange(s+1, d):
			current_cost = generate_memoized_table(s, i, memoized_cache) + generate_memoized_table(i, d, memoized_cache)
			if current_cost < minimum_cost:
				minimum_cost = current_cost

		memoized_cache[s][d] = minimum_cost
		return minimum_cost

	# Create a memoization table of minimum costs between any two stations, on the first call
	# a memoization table (0, n-1) calculates all the minimum costs between any two stations i, j s.t. i != j
	# The same table will be used for subsequent calls unless explicitly reset
	if not hasattr(minCost_memoized, "memoized_cache"):
		setattr(minCost_memoized, "memoized_cache", [[None if i != j else 0 for i in xrange(len(cost))] for j in xrange(len(cost))])
		generate_memoized_table(0, len(cost)-1, minCost_memoized.memoized_cache)

	# Return the queried cost between stations s,d using the memoization table
	return minCost_memoized.memoized_cache[s][d]


'''
Dynamic programming solution: O(n^3) runtime, O(n^2) space
	Uses the recurrence relation to build a bottom-up table to avoid recursion

	minCost(0, n-1) = min{ cost[0][n-1],
						   cost[0][1] + T[1][n-1],
						   T[0][2] + T[2][n-1],
						   . . .
						   T[0][n-2) + cost[n-2][n-1]
						  }
					 = min{ T[0][n-1],
						   T[0][1] + T[1][n-1],
						   T[0][2] + T[2][n-1],
						   . . .
						   T[0][n-2) + T[n-2][n-1]
						  } 
						  where T[i][i] = 0, and T[i][i+1] = cost[i][i+1]
						  T[i][j] stores the minimum cost between two stations i and j, i <= j


	Start filling up the 2D table with minimum cost between stations 1 stop apart, then 2 stops apart, ...
	This is so T[s][d] needs values that are utmost d-s-1 apart, and will be available for calculation.

Sample run:
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]

	Fill length 0
	|   | 1 |  1 |  2 |  3 |
	|---+---+----+----+----|
	| 0 | 0 |    |    |    |	T[0][0]
	| 1 |   |  0 |    |    |	T[1][1]
	| 2 |   |    |  0 |    |	T[2][2]
	| 3 |   |    |    |  0 |	T[3][3]

	Fill length 1
	|   | 1 |  1 |  2 |  3 |
	|---+---+----+----+----|
	| 0 | 0 | 15 |    |    |	T[0][1]
	| 1 |   |  0 | 40 |    |	T[1][2]
	| 2 |   |    |  0 | 70 |	T[2][3]
	| 3 |   |    |    |  0 |

	Fill length 2
	|   | 1 |  1 |  2 |  3 |
	|---+---+----+----+----|
	| 0 | 0 | 15 | 55 |    |	T[0][2] = min(T[0][2], T[0][1]+T[1][2]) = min(80, 15+40) == 55
	| 1 |   |  0 | 40 | 50 |	T[1][3] = min(T[1][3], T[1][2]+T[2][3]) = min(50, 40+70) == 50
	| 2 |   |    |  0 | 70 |
	| 3 |   |    |    |  0 |

	Fill length 3
	|   | 1 |  1 |  2 |  3 |
	|---+---+----+----+----|
	| 0 | 0 | 15 | 55 | 65   |	T[0][3] = min(T[0][3], T[0][1]+T[1][3], T[0][2]+T[2][3]) = min(90, 15+50, 80+70) == 65
	| 1 |   |  0 | 40 | 50 |	
	| 2 |   |    |  0 | 70 |
	| 3 |   |    |    |  0 |
'''
def minCost_DP(s, d, cost):
	n = len(cost)

	# Create a bottom-up DP table of minimum costs between any two stations, on the first call
	# a DP table (0, n-1) calculates all the minimum costs between any two stations i, j s.t. i != j
	# The same table will be used for subsequent calls unless explicitly reset
	if not hasattr(minCost_DP, "T"):
		setattr(minCost_DP, "T", [[None if i != j else 0 for i in xrange(len(cost))] for j in xrange(len(cost))])

		T = minCost_DP.T
		step = 0
		for i in xrange(0, n):
			for j in xrange(0, n-step):
				T[j][j+step] =  cost[j][j+step]
				for k in xrange(j+1, j+step):
					if T[j][k] + T[k][j+step] < T[j][j+step]:
						T[j][j+step] = T[j][k] + T[k][j+step] 
			step += 1

	# Return the queried cost between stations s,d using the DP table
	return minCost_DP.T[s][d]

	




'''
Dynamic programming solution:
	To end station(n-1) from start station(0)
	If only the minimum cost to end station from start station is needed,
	Then the previous dynamic programming table is slightly redundant as it calculates minimum cost between all station pairs i, j (i <= j)

Algorithm Outline:
	If minimum cost to station i (from station 0) is known, then minimum cost to station i+1 is
		min (previously calculated mincost[i+1], mincost[i]+cost[i][i+1]
	We start at station 0, and calculate mincost[1..n-1] from station 0
	Then move on to station 1, and calculate mincost[2..n-1] from station 1
	At iteration i, mincost[i] is completely resolved, as we'd have effectively calculated
	  min { cost[0][i],
	        mincost[1] + cost[1][i],
			mincost[2] + cost[2][i],
			. . .
			mincost[i-1] + cost[i-1][i]]
		  }
	After iteration n-1, we'd have found the minimum cost to station (n-1) from 0.
	NOTE: We would also have minimum costs to each station from station 0 as part of this optimized DP.

Sample run:
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]
	T = [0, 15, 80, 90]

	i: 1
	  T[2] = 80
	  T[1] + cost[1][2] = 15+40 == 55
	  T[2] = 55
	  T = [0, 15, 55, 90]
	  --
	  T[3] = 90
	  T[1] + cost[1][3] = 15 + 50 == 65
	  T[3] = 65
	  T = [0, 15, 55, 65]

	i: 2
	  T[3] = 65
	  T[2] + cost[2][3] = 55 + 70 == 125 > 65
	  T = [0, 15, 55, 65]
'''
def minCost_DP_from_start(cost, d=None):
	n = len(cost)

	if not hasattr(minCost_DP_from_start, "T"):
		# Initialize all station minimum costs to costs from station 0
		setattr(minCost_DP_from_start, "T", cost[0][:])

		# Fill DP table updating min costs from station 0 to each station 1 .. n-1
		T = minCost_DP_from_start.T
		for i in xrange(1, n):
			for j in xrange(i+1, n):
				if T[j] > T[i] + cost[i][j]:
					T[j] = T[i] + cost[i][j]

	# By default, Return minimum cost to end station
	if d == None:
		d = n-1
	return minCost_DP_from_start.T[d]





if __name__ == '__main__':
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]

	assert minCost(0, 3, cost) == 65
	assert minCost(1, 3, cost) == 50
	assert minCost(0, 2, cost) == 55

	assert minCost_memoized(0, 2, cost) == 55 # First call creates the memoization table
	assert minCost_memoized.memoized_cache == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]
	assert minCost_memoized(1, 3, cost) == 50
	assert minCost_memoized(0, 3, cost) == 65

	assert minCost_DP(0, 3, cost) == 65
	assert minCost_DP.T == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]
	assert minCost_DP(1, 3, cost) == 50
	assert minCost_DP(0, 3, cost) == 65 # consistency is important! :)
	assert minCost_DP(0, 2, cost) == 55

	assert minCost_DP_from_start(cost) == 65
	assert minCost_DP_from_start.T == [0, 15, 55, 65]
	assert minCost_DP_from_start(cost, 1) == 15
	assert minCost_DP_from_start(cost, 2) == 55
	assert minCost_DP_from_start(cost, 3) == 65

	cost = [
				[0, 10, 75, 94],
				[-1, 0, 35, 50],
				[-1, -1, 0, 80],
				[-1, -1, -1, 0]
			]

	# Reset previous memoized and DP tables for new station cost matrix
	delattr(minCost_memoized, "memoized_cache")
	delattr(minCost_DP, "T")
	delattr(minCost_DP_from_start, "T")

	assert minCost(0, 3, cost) == 60
	assert minCost(1, 3, cost) == 50
	assert minCost(0, 2, cost) == 45

	assert minCost_memoized(0, 2, cost) == 45 # First call creates the memoization table
	assert minCost_memoized.memoized_cache == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert minCost_memoized(1, 3, cost) == 50
	assert minCost_memoized(0, 3, cost) == 60

	assert minCost_DP(0, 3, cost) == 60
	assert minCost_DP.T == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert minCost_DP(1, 3, cost) == 50
	assert minCost_DP(0, 3, cost) == 60 # consistency is important! :)
	assert minCost_DP(0, 2, cost) == 45

	assert minCost_DP_from_start(cost) == 60
	assert minCost_DP_from_start.T == [0, 10, 45, 60]
	assert minCost_DP_from_start(cost, 1) == 10
	assert minCost_DP_from_start(cost, 2) == 45
	assert minCost_DP_from_start(cost, 3) == 60


