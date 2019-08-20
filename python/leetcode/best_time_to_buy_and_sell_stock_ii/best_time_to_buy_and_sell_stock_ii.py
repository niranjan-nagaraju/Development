'''
https://leetcode.com/problems/best-time-to-buy-and-sell-stock-ii/


Say you have an array for which the ith element is the price of a given stock on day i.

Design an algorithm to find the maximum profit. You may complete as many transactions as you like (i.e., buy one and sell one share of the stock multiple times).

Note: You may not engage in multiple transactions at the same time (i.e., you must sell the stock before you buy again).

Example 1:
Input: [7,1,5,3,6,4]
Output: 7
Explanation: Buy on day 2 (price = 1) and sell on day 3 (price = 5), profit = 5-1 = 4.
             Then buy on day 4 (price = 3) and sell on day 5 (price = 6), profit = 6-3 = 3.

Example 2:
Input: [1,2,3,4,5]
Output: 4
Explanation: Buy on day 1 (price = 1) and sell on day 5 (price = 5), profit = 5-1 = 4.
             Note that you cannot buy on day 1, buy on day 2 and sell them later, as you are
             engaging multiple transactions at the same time. You must sell before buying again.

Example 3:
Input: [7,6,4,3,1]
Output: 0
Explanation: In this case, no transaction is done, i.e. max profit = 0.
'''



'''
Solution:
	Create deltas between consecutive prices.
	Add them up to count local maxima/profit, until we find a price on day i, that reduces current profit.
	  at this point, we complete the trade {buy, day (i-1)}, and start a fresh buy at day i, with local profit = 0
    Otherwise, if day i price adds to profit, keep running the transaction adding day i's price to profit.
	Repeat till end.


TC1:
	prices: [1 2 3 4]
	deltas: [1 1 1]
	current profit: 0
	profit : 0
	buy_ = 1

	iter #1:
		current profit: +1
		profit: +1
	iter #2:
		current profit: +2
		profit: +2
	iter #3:
		current profit: +3
		profit: +3
	Final - total profit: 3
	Txns: [((1, 4,), 3)]


TC2:
	prices: [4 3 2 1]
	deltas: [-1 -1 -1]
	buy_ = 4

	current profit: 0
	profit : 0

	iter #1:
		current profit: -1 < profit
		buy_ = 3
		current profit = 0
	iter #2:
		current profit: -2 < profit
		buy_ = 2
		current profit = 0
	iter #3:
		current profit: -3 < profit
		buy_ = 1
		current profit = 0
	Final - profit: 0
	Txns: []


TC3:
	prices: [2 5 1 7]
	deltas: [3 -4 6]

	current profit: 0
	profit : 0
	buy_: 2

	iter #1:
		current profit: 3 > profit
		profit = 3
	iter #2:
		current profit: -1 < profit
		  book profits for previous day
		  Txns: (2, 5), 3
		  buy_ = 1
		  profit = 0
		  current profit = 0
	iter #3:
		current profit: 6 > profit
		  profit = 6

	END:
	   profit > 0 => Txns: [ ((2,5),3), (1,7),6)]
	   Total profit: 9
'''

class Solution(object):
	def maxProfit(self, prices):
		"""
		:type prices: List[int]
		:rtype: int
		"""

		if not prices:
			return 0

		buy = prices[0]
		profit= 0
		curr_profit = 0
		total_profit = 0
		
		for i in range(1, len(prices)):
			curr_profit += prices[i] - prices[i-1]

			# Current profit increases by holding the position for day i
			if curr_profit > profit:
				profit = curr_profit
			else:
				# Price has dipped on day i
				# Book profits by selling on day {i-1} if there was a profit
				if profit > 0:
					total_profit += profit
				buy = prices[i]
				curr_profit = 0
				profit = 0

		# Close as yet unclosed positions that might yield in a profit
		if profit > 0:
			total_profit += profit

		return total_profit



if __name__ == '__main__':
	s = Solution()
	assert s.maxProfit([1,2,3,4]) == 3
	assert s.maxProfit([4,3,2,1]) == 0
	assert s.maxProfit([2,5,1,7]) == 9
	assert s.maxProfit([7,1,5,3,6,4]) == 7
	assert s.maxProfit([1,2,3,4,5]) == 4
	assert s.maxProfit([7,6,4,3,1]) == 0
	assert s.maxProfit(range(1,8)) == 6
	assert s.maxProfit(range(7, 0, -1)) == 0
	assert s.maxProfit([2,5,1,3]) == 5
	assert s.maxProfit([1,5,2,7]) == 9
	assert s.maxProfit([7,6,5,4,5]) == 1
	assert s.maxProfit([7,6,5,4,6,1,4]) == 5
	assert s.maxProfit([7,2,5,3,6,4,1,7]) == 12
	assert s.maxProfit([7,1,5,3,6,4]) == 7
	assert s.maxProfit([1,3,2,5]) == 5

