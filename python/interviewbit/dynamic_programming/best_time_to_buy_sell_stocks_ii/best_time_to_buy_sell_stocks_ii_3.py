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
		  Mark A[i] as new buy price (we found a better buy price)
		  Profit for Day i: 0 (We didn't sell any)
		Otherwise,
			if A[i] >= A[i-1]
				Sell A[i], Register A[i]-A[i-1] as Profit for Day i
				Buy A[i]
			{A[i] == A[i-1] can be registered as a sell with 0 profit on Day i}
	2. Max profit: sum of all items in Profits DP table


NOTE: We don't need to keep track of profits at the end of each day
	1. Day 0, Mark A[0] as buy price, we don't have a sell price for it yet
		max_profits: 0
	2. At Day i, ( 0 < i < n )
		if A[i] < A[i-1]
			close previous position if there's a sell, add to max_profits
			Mark Day i's price, A[i] as buy
			Mark sell price as 'Nothing', we don't have anything to sell at this point.
		otherwise,
			Mark A[i] as tentative sell 
			NOTE: Tentative sells ensure if we have an increasing sequence, we
					continue holding the position until we can realize max profit in a single txn)
				{e.g, [1,2,3,4],
					buy at 1, sell at 4 instead of buying at 1, sell 2, buy 2, sell 3, buy 3 and sell 4}
				This yields the same profit with lesser number of txns, so if each txn has a fee, this reduced # of txns helps.
	3. Return accumulated max_profits

Sample run:
	   0 1 2 3 4 5 6
	A: 5 1 4 3 2 6 8
	max_profits: 0
	
	Day 0:
		buy: 5
		sell: None

	Day 1:
		price: 1 < Day 0
		No sell price => no txn
		buy: 1
	
	Day 2:
		price: 4 > Day 1
		tentative sell: 4

	Day 3:
		price: 3 < Day 2
		sell: 4, buy: 1
		max_pofits: +(4-1) == +3 == 3
		buy: 3
		sell: None
	
	Day 4:
		price: 2 < Day 3
		No sell price => no txn
		buy : 2

	Day 5:
		price: 6 > Day 4
		tentative sell: 6

	Day 6:
		price: 8 > Day 5
		tentative sell: 8

	EOF:
		sell: 8, buy: 2
		close position on Day 6
		max_profits: +(8-2) == +6 == 9

	max_profits: 9
'''
class Solution:
	def max_profits_any_txns(self, A):
		if not A:
			return 0

		buy = A[0]
		sell = None
		max_profits = 0

		for i in xrange(1, len(A)):
			if A[i] < A[i-1]:
				# Close previous position if it is
				# in profit
				if sell is not None:
					sell = A[i-1]
					max_profits += (sell-buy)
					sell = None
				buy = A[i]
			else:
				sell = A[i]

		
		# At the end, check if we have a sell price
		# if yes, execute a txn with (sell,buy)
		if sell is not None:
			max_profits += (sell-buy)

		return max_profits


if __name__ == '__main__':
	s = Solution()
	assert s.max_profits_any_txns([1,2,3]) == 2
	assert s.max_profits_any_txns([5,2,10]) == 8
	assert s.max_profits_any_txns([5,1,4,3,2,6,8]) == 9
	assert s.max_profits_any_txns([7,1,5,3,6,4]) == 7
	assert s.max_profits_any_txns([7,6,4,2]) == 0
	assert s.max_profits_any_txns([1,5,2,7]) == (4+5)

