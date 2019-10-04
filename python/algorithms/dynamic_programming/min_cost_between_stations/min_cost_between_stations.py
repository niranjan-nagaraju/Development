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


class MinCost(object):
	def __init__(self, cost):
		# cost matrix between stations
		self.cost = cost

		# NxN memoized table that caches minimum cost between any two stations s,d, s <= d
		# a memoized table for 0->N-1 will involve all s,d combinations
		# and therefore will have minimum cost between any two stations s,d s<=d
		self.memoized_table = None

		# NxN DP table calculated that offers a minimum cost between any two stations s,d, s <= d
		self.dp_table = None

		# An optimized bottom-up DP table (1xN) that calculates minimum cost from starting station 0 to any other end station
		self.dp_table_from_start = None



	'''
	Recursive Solution outline: (inefficient), O(2^n)
		minCost(0, n-1) = min{ cost[0][n-1],
							   cost[0][1] + minCost(1, n-1),
							   minCost(0,2) + minCost(2, n-1),
							   . . .
							   minCost(0, n-2) + cost[n-2][n-1]
							  }
	'''
	def calculate_mincost(self, s, d):
		if (s == d) or (s == d-1):
			return self.cost[s][d]

		minimum_cost = self.cost[s][d]
		for i in xrange(s+1, d):
			current_cost = self.calculate_mincost(s, i) + self.calculate_mincost(i, d)
			if current_cost < minimum_cost:
				minimum_cost = current_cost

		return minimum_cost



	'''
	Memoized solution:
		Use the same recurrence relation, but memoize intermediate results
		so they don't get recomputed needlessly
		O(n^3) runtime, O(n^2) space
	NOTE: The memoization table will have to be recalculated using this method if the cost matrix between stations, changes.
	'''
	# Generate a memoization table
	def generate_memoized_table(self):
		def generate_memoized_table_r(s, d, memoized_cache):
			if (s == d) or (s == d-1):
				memoized_cache[s][d] = self.cost[s][d]
				return memoized_cache[s][d]

			minimum_cost = self.cost[s][d]
			for i in xrange(s+1, d):
				current_cost = generate_memoized_table_r(s, i, memoized_cache) + generate_memoized_table_r(i, d, memoized_cache)
				if current_cost < minimum_cost:
					minimum_cost = current_cost

			memoized_cache[s][d] = minimum_cost
			return minimum_cost

		# Call the helper function to find mincost between 0 to n-1
		# which in turn fills the memoization table between any 2 stations s,d
		# while these values are calculated top-down
		self.memoized_table = [[None if i != j else 0 for i in xrange(len(self.cost))] for j in xrange(len(self.cost))]
		generate_memoized_table_r(0, len(self.cost)-1, self.memoized_table)
		return self.memoized_table


	# Use memoization to calculate mincost between any two stations i, j i <= j
	def calculate_mincost_memoized(self, s, d):
		# Create a memoization table of minimum costs between any two stations, on the first call
		# a memoization table (0, n-1) calculates all the minimum costs between any two stations i, j s.t. i != j
		if not self.memoized_table:
			self.generate_memoized_table()

		# Return the queried cost between stations s,d using the memoization table
		return self.memoized_table[s][d]


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
		| 0 | 0 | 15 | 55 | 65 |	T[0][3] = min(T[0][3], T[0][1]+T[1][3], T[0][2]+T[2][3]) = min(90, 15+50, 80+70) == 65
		| 1 |   |  0 | 40 | 50 |	
		| 2 |   |    |  0 | 70 |
		| 3 |   |    |    |  0 |
	NOTE: The DP table will have to be recalculated using this method if the cost matrix between stations, changes.
	'''
	def generate_dp_table(self):
		n = len(self.cost)

		# Create a bottom-up DP table of minimum costs between any two stations
		# a DP table (0, n-1) calculates all the minimum costs between any two stations i, j s.t. i != j
		# The same table will be used for subsequent calls unless explicitly reset
		self.dp_table = [[None if i != j else 0 for i in xrange(len(self.cost))] for j in xrange(len(self.cost))]

		T = self.dp_table
		step = 0
		for i in xrange(0, n):
			for j in xrange(0, n-step):
				T[j][j+step] =  self.cost[j][j+step]
				for k in xrange(j+1, j+step):
					if T[j][k] + T[k][j+step] < T[j][j+step]:
						T[j][j+step] = T[j][k] + T[k][j+step] 
			step += 1

		return T


	# Use previously generated DP table to calculate mincost between any two stations i, j i <= j
	def calculate_mincost_DP(self, s, d):
		# Create a DP table of minimum costs between any two stations, on the first call
		if not self.dp_table:
			self.generate_dp_table()

		# Return the queried cost between stations s,d using the DP table
		return self.dp_table[s][d]

	


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
	NOTE: The DP table will have to be recalculated using this method if the cost matrix between stations, changes.
	'''
	def generate_dp_table_from_start(self):
		n = len(self.cost)

		# Initialize all station minimum costs to costs from station 0
		self.dp_table_from_start = self.cost[0][:]

		# Fill DP table updating min costs from station 0 to each station 1 .. n-1
		T = self.dp_table_from_start
		for i in xrange(1, n):
			for j in xrange(i+1, n):
				if T[j] > T[i] + self.cost[i][j]:
					T[j] = T[i] + self.cost[i][j]

		return T

	# Use previously generated DP table to calculate mincost to any station i from station 0
	def calculate_mincost_from_start_DP(self, d=None):
		# Create a DP table of minimum costs between to each station i, from station 0,  on the first call
		if not self.dp_table_from_start:
			self.generate_dp_table_from_start()

		# By default, Return minimum cost to end station
		if d == None:
			d = len(self.cost)-1
		return self.dp_table_from_start[d]





if __name__ == '__main__':
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]

	m = MinCost(cost)

	assert m.calculate_mincost(0, 3) == 65
	assert m.calculate_mincost(1, 3) == 50
	assert m.calculate_mincost(0, 2) == 55

	assert m.calculate_mincost_memoized(0, 2) == 55 # First call creates the memoization table
	assert m.memoized_table == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]
	assert m.calculate_mincost_memoized(1, 3) == 50
	assert m.calculate_mincost_memoized(0, 3) == 65
	assert m.generate_memoized_table() == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]


	assert m.calculate_mincost_DP(0, 3) == 65
	assert m.dp_table == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]
	assert m.calculate_mincost_DP(1, 3) == 50
	assert m.calculate_mincost_DP(0, 3) == 65 # consistency is important! :)
	assert m.calculate_mincost_DP(0, 2) == 55
	assert m.generate_dp_table() == [
				[0, 15, 55, 65],
				[None, 0, 40, 50],
				[None, None, 0, 70],
				[None, None, None, 0]
			]


	assert m.calculate_mincost_from_start_DP() == 65
	assert m.dp_table_from_start == [0, 15, 55, 65]
	assert m.calculate_mincost_from_start_DP(1) == 15
	assert m.calculate_mincost_from_start_DP(2) == 55
	assert m.calculate_mincost_from_start_DP(3) == 65
	assert m.generate_dp_table_from_start() == [0, 15, 55, 65]

	cost = [
				[0, 10, 75, 94],
				[-1, 0, 35, 50],
				[-1, -1, 0, 80],
				[-1, -1, -1, 0]
			]

	m2 = MinCost(cost)
	assert m2.calculate_mincost(0, 3) == 60
	assert m2.calculate_mincost(1, 3) == 50
	assert m2.calculate_mincost(0, 2) == 45

	assert m2.calculate_mincost_memoized(0, 2) == 45 # First call creates the memoization table
	assert m2.memoized_table == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert m2.calculate_mincost_memoized(1, 3) == 50
	assert m2.calculate_mincost_memoized(0, 3) == 60
	assert m2.generate_memoized_table() == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert m2.calculate_mincost_memoized(0, 3) == 60


	assert m2.calculate_mincost_DP(0, 3) == 60
	assert m2.dp_table == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert m2.calculate_mincost_DP(1, 3) == 50
	assert m2.calculate_mincost_DP(0, 3) == 60 # consistency is important! :)
	assert m2.calculate_mincost_DP(0, 2) == 45
	assert m2.generate_dp_table() == [
				[0, 10, 45, 60],
				[None, 0, 35, 50],
				[None, None, 0, 80],
				[None, None, None, 0]
			]
	assert m2.calculate_mincost_DP(0, 3) == 60 # consistency is important! :)

	assert m2.calculate_mincost_from_start_DP() == 60
	assert m2.dp_table_from_start == [0, 10, 45, 60]
	assert m2.calculate_mincost_from_start_DP(1) == 10
	assert m2.calculate_mincost_from_start_DP(2) == 45
	assert m2.calculate_mincost_from_start_DP(3) == 60
	assert m2.generate_dp_table_from_start() == [0, 10, 45, 60]
	assert m2.calculate_mincost_from_start_DP(3) == 60


