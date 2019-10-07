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
Top-Down Memoized solution:
	Uses the recurrence relation, but caches intermediate results so they arent recalculated over and over needlessly.
		minCost(0, n-1) = min{ cost[0][n-1],
							   cost[0][1] + minCost(1, n-1),
							   minCost(0,2) + minCost(2, n-1),
							   . . .
							   minCost(0, n-2) + cost[n-2][n-1]
							  }
'''

class MinCost(object):
	def __init__(self, cost):
		# cost matrix between stations
		self.cost = cost

		# NxN memoized table that caches minimum cost between any two stations s,d, s <= d
		# a memoized table for 0->N-1 will involve all s,d combinations
		# and therefore will have minimum cost between any two stations s,d s<=d
		self.memoized_table = None



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




if __name__ == '__main__':
	cost = [
				[0, 15, 80, 90],
				[-1, 0, 40, 50],
				[-1, -1, 0, 70],
				[-1, -1, -1, 0]
			]

	m = MinCost(cost)
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

	cost = [
				[0, 10, 75, 94],
				[-1, 0, 35, 50],
				[-1, -1, 0, 80],
				[-1, -1, -1, 0]
			]

	m2 = MinCost(cost)
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


