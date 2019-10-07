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
Dynamic Programming Solution:
	Builds a nxn bottom-up DP table calculating minimum cost between any two stations, s,d s <= d
'''
class MinCost(object):
	def __init__(self, cost):
		# cost matrix between stations
		self.cost = cost

		# NxN DP table calculated that offers a minimum cost between any two stations s,d, s <= d
		self.dp_table = None


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

	



if __name__ == '__main__':
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]

	m = MinCost(cost)
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

	cost = [
				[0, 10, 75, 94],
				[-1, 0, 35, 50],
				[-1, -1, 0, 80],
				[-1, -1, -1, 0]
			]

	m2 = MinCost(cost)

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


