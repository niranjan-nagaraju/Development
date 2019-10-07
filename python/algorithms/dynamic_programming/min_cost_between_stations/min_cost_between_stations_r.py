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
Brute force solution:
	Uses the recurrence relation
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


