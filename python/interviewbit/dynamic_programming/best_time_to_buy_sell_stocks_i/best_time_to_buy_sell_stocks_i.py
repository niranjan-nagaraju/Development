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


Sample run 1:
	A: [1, 4, 5, 2, 4]

	P: [0, 0, 0, 0, 0]

	buy: A[0] = 1

	Day 1:
		Price: 4 > Day 0
		Sell: P[1] = (4-1) = 3
		P: [0, 3, 0, 0, 0]


	Day 2:
		Price: 5 > Day 1
		sell: P[2] = (5-1) = 4
		P: [0, 3, 4, 0, 0]

	Day 3:
		Price: 2 < Day 2
			2 < buy price? NO
		sell: P[3] = (2-1) = 1
		P: [0, 3, 4, 1, 0]

	Day 4:
		Price: 4 > Day 3
		sell: P[4] = (4-1) = 3
		P: [0, 3, 4, 1, 3]
		
	max(P) = 4 => Max profit


Sample run 2:
	A: [2, 5, 1, 5, 7]

	buy: A[0] = 2
	P: [0, 0, 0, 0, 0]

	Day 1:
		price: 5 > buy
		sell: P[1] = (5-2) = 3
		P: [0, 3, 0, 0, 0]

	Day 2:
		price: 1 < Day 1
			1 < buy => buy: 1
		P: [0, 3, 0, 0, 0]

	Day 3:
		price: 5 > Day 2
		sell: P[3] = (5-1) = 4
		P: [0, 3, 0, 4, 0]

	Day 4:
		price: 7 > Day 3
		sell: P[4] = (7-1) = 6
		P: [0, 3, 0, 4, 6]

	max(P): 6 => Max profit

NOTE: The 0s indicate the buy prices for the profit txns on the right until another lower buy price is encountered,
	in which case, profit for that day i, where a better lower price is seen will be 0.
	To trace back the max profit transaction,
	  Go backwards from max profit index to an index where a 0 is found.
	  In the sample run 2,
		A: [2, 5, 1, 5, 7]
		max profit : 6, index: 4
		sell price = A[index] = A[4] = 7
		0 to its left, is at index=2
		buy price = A[index] = A[2] = 1

		buy at 1
		sell at 7
		max profit: 6
'''
class Solution:
	def max_profit_1txn(self, A):
		if not A:
			return 0

		buy = A[0]
		P = [0]*len(A)
		for i in xrange(1, len(A)):
			if A[i] < A[i-1]:
				if A[i] < buy:
					buy = A[i]
			P[i] = (A[i]-buy)

		print P
		max_profit = max(P)
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


