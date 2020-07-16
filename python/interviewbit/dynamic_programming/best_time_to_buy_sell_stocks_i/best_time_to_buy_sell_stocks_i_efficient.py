'''
https://www.interviewbit.com/problems/best-time-to-buy-and-sell-stocks-i/

Best Time to Buy and Sell Stocks I

Problem Description
Say you have an array, A, for which the ith element is the price of a given stock on day i.
If you were only permitted to complete at most one transaction (i.e, buy one and sell one share of the stock), design an algorithm to find the maximum profit.
Return the maximum possible profit.

Problem Constraints
0 <= len(A) <= 7e5
1 <= A[i] <= 1e7

Input Format
The first and the only argument is an array of integers, A.

Output Format
Return an integer, representing the maximum possible profit.

Example Input
Input 1:
 A = [1, 2]
Input 2:
 A = [1, 4, 5, 2, 4]


Example Output
Output 1:
 1
Output 2:
 4

Example Explanation
Explanation 1:
 Buy the stock on day 0, and sell it on day 1.
Explanation 2:
 Buy the stock on day 0, and sell it on day 2.
'''


'''
Solution Outline:
	1. Initially, set A[0] as buy price
	2. A[i] is the price on Day i,
		P[]: DP table registering the profit for Day i.
	3. On Day i, (0 < i < n)
		If A[i] < A[i-1]
			Check if A[i] < buy price:
				Mark Day i's price as buy (we found a better buy price)
				buy: A[i]
			Otherwise:
				A[i] is still greater than buy price
				Register a sell transaction, P[i] = (A[i]-buy)
		Otherwise, A[i] >= A[i-1]
				Register a sell transaction, P[i] = (A[i]-buy)
	4. Return max(P) as the single transaction with max profit


Memory efficient DP:
	Avoid using DP table, P, by updating max_profit as we register each sell txn.

Sample run:
	A: [1, 4, 5, 2, 4]

	buy: A[0] = 1
	max_profit = 0

	Day 1:
		Price: 4 > Day 0
		Sell: (4-1) = 3 > max_profit
		=> max_profit = 3


	Day 2:
		Price: 5 > Day 1
		sell: (5-1) = 4 > max_profit
		=> max_profit = 4

	Day 3:
		Price: 2 < Day 2
			2 < buy price? NO
		sell: (2-1) = 1 < max_profit

	Day 4:
		Price: 4 > Day 3
		sell: (4-1) = 3 < max_profit

	max_profit = 4
'''
class Solution:
	def max_profit_1txn(self, A):
		if not A:
			return 0

		buy = A[0]
		max_profit = 0
		for i in xrange(1, len(A)):
			if A[i] < A[i-1]:
				if A[i] < buy:
					buy = A[i]
			max_profit = max(A[i]-buy, max_profit)

		return max_profit


if __name__ == '__main__':
	s = Solution()
	assert s.max_profit_1txn([1,4,5,2,4]) == 4
	assert s.max_profit_1txn([1,2,3]) == 2
	assert s.max_profit_1txn([5,2,10]) == 8
	assert s.max_profit_1txn([5,1,4,3,2,6,8]) == 7
	assert s.max_profit_1txn([7,1,5,3,6,4]) == 5
	assert s.max_profit_1txn([7,6,4,2]) == 0
	assert s.max_profit_1txn([2,5,1,7]) == 6
	assert s.max_profit_1txn([2,5,1,3]) == 3
	assert s.max_profit_1txn([1,5,2,7]) == 6

	assert s.max_profit_1txn(range(1,8)) == 6
	assert s.max_profit_1txn(range(7, 0, -1)) == 0
	assert s.max_profit_1txn([7,6,5,4,5]) == 1
	assert s.max_profit_1txn([7,6,5,4,6,1,4]) == 3
	assert s.max_profit_1txn([7,2,5,3,6,4,1,7]) == 6
	assert s.max_profit_1txn([7,1,5,3,6,4]) == 5
	assert s.max_profit_1txn([7,6,4,3,1]) == 0


