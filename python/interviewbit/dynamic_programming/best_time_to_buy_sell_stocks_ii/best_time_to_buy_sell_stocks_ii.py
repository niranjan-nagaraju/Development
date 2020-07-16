'''
https://www.interviewbit.com/problems/best-time-to-buy-and-sell-stocks-ii/

Best Time to Buy and Sell Stocks II

Say you have an array, A, for which the ith element is the price of a given stock on day i.
Design an algorithm to find the maximum profit.
You may complete as many transactions as you like (i.e., buy one and sell one share of the stock multiple times).
However, you may not engage in multiple transactions at the same time (ie, you must sell the stock before you buy again).

Input Format:
The first and the only argument is an array of integer, A.

Output Format:
Return an integer, representing the maximum possible profit.

Constraints:
1 <= len(A) <= 1e5
1 <= A[i] <= 1e7

Example :
Input 1:
    A = [1, 2, 3]
Output 1:
    2
Explanation 1:
    => Buy a stock on day 0.
    => Sell the stock on day 1. (Profit +1)
    => Buy a stock on day 1.
    => Sell the stock on day 2. (Profit +1)
    Overall profit = 2

Input 2:
    A = [5, 2, 10]
Output 2:
    8
Explanation 2:
    => Buy a stock on day 1.
    => Sell the stock on on day 2. (Profit +8)
    Overall profit = 8
'''

'''
Solution Outline:
	0. Initially, Set A[0] as buy price
	1. A[i] is the price on Day i
		if A[i] < A[i-1]
		  Mark A[i-1] as new buy price (we found a better buy price)
		  Profit for Day i: 0 (We didn't sell any)
		Otherwise,
			if A[i] >= A[i-1]
				Sell A[i], Register A[i]-A[i-1] as Profit for Day i
				Buy A[i]
			{A[i] == A[i-1] can be registered as a sell with 0 profit on Day i}
	2. Max profit: sum of all items in Profits DP table


Sample run:
	   0 1 2 3 4 5 6
	A: 5 1 4 3 2 6 8
	P: 0 0 0 0 0 0 0  {max profits at the end of Day i} 
	
	Day 0:
		buy: 5
		sell: None
		P: 0 0 0 0 0 0 0

	Day 1:
		price: 1 < Day 0
		buy: 1
		P: 0 0 0 0 0 0 0
	
	Day 2:
		price: 4 > Day 1
		sell: 4
		profit: 4-1 = 3
		buy: 4
		P: 0 0 3 0 0 0 0

	Day 3:
		price: 3 < Day 2
		buy: 3
		P: 0 0 3 0 0 0 0
	
	Day 4:
		price: 2 < Day 3
		buy : 2
		P: 0 0 3 0 0 0 0

	Day 5:
		price: 6 > Day 4
		sell: 6
		profit: 6-2 = 4
		buy: 6
		P: 0 0 3 0 0 4 0

	Day 6:
		price: 8 > Day 5
		sell: 8
		profit: 8-6 = 2
		P: 0 0 3 0 0 4 2

	Max profit: 3+4+2 = 8
'''
class Solution:
	def buy_sell_stocks(self, A):
		if not A:
			return 0

		buy = A[0]
		sell = None
		P = [0]*len(A) # profits table for each day

		for i in xrange(1, len(A)):
			if A[i] < A[i-1]:
				buy = A[i]
			else:
				P[i] = A[i]-buy
				buy = A[i]

		# print P
		max_profits = sum(P)
		return max_profits


if __name__ == '__main__':
	s = Solution()
	assert s.buy_sell_stocks([1,2,3]) == 2
	assert s.buy_sell_stocks([5,2,10]) == 8
	assert s.buy_sell_stocks([5,1,4,3,2,6,8]) == 9
	assert s.buy_sell_stocks([7,1,5,3,6,4]) == 7
	assert s.buy_sell_stocks([7,6,4,2]) == 0

