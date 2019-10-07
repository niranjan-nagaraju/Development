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
Optimized Dyanamic Programming solution:
	If we all we need is the minimum cost needed to reach station (n-1) from station 0,
	Then we dont need intermediate minimum costs between any two stations s, d
	Just the minimum costs from station 0 to all other stations 1, 2, .. n-1 suffices.
	mincost to station n-1 = mincost to station n + cost[n-2][n-1]

	Build a bottom-up 1xn DP-table with minimum costs from station 0 to each intermediate station i, 0 < i < n
	NOTE: This also means that we no longer would be able to answer minimum cost queries between any two stations s,d
	Just (0, d) 0 <= d <= n-1 
'''
class MinCost(object):
	def __init__(self, cost):
		# cost matrix between stations
		self.cost = cost

		# An optimized bottom-up DP table (1xN) that calculates minimum cost from starting station 0 to any other end station
		self.dp_table_from_start = None


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
	assert m2.calculate_mincost_from_start_DP() == 60
	assert m2.dp_table_from_start == [0, 10, 45, 60]
	assert m2.calculate_mincost_from_start_DP(1) == 10
	assert m2.calculate_mincost_from_start_DP(2) == 45
	assert m2.calculate_mincost_from_start_DP(3) == 60
	assert m2.generate_dp_table_from_start() == [0, 10, 45, 60]
	assert m2.calculate_mincost_from_start_DP(3) == 60


