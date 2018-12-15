'''
https://leetcode.com/problems/best-time-to-buy-and-sell-stock/

Say you have an array for which the ith element is the price of a given stock on day i.

If you were only permitted to complete at most one transaction (i.e., buy one and sell one share of the stock), design an algorithm to find the maximum profit.

Note that you cannot sell a stock before you buy one.

Example 1:

Input: [7,1,5,3,6,4]
Output: 5
Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6), profit = 6-1 = 5.
             Not 7-1 = 6, as selling price needs to be larger than buying price.

Example 2:

Input: [7,6,4,3,1]
Output: 0
Explanation: In this case, no transaction is done, i.e. max profit = 0.


Solution:
	Create deltas between consecutive prices.
	Add them up to count current profit, until we find a lesser buy price
		when that happens, we start counting new current profit considering the new buy price
	anytime, the current profit (based on a buy price) exceeds max profit, replace max profit


TC1:
	prices: [1 2 3 4]
	deltas: [1 1 1]
	current profit: 0
	mac profit : 0

	iter #1:
		current profit: +1
		max profit: +1
	iter #2:
		current profit: +2
		max profit: +2
	iter #3:
		current profit: +3
		max profit: +3
	Final - max profit: 3


TC2:
	prices: [4 3 2 1]
	deltas: [-1 -1 -1]

	current profit: 0
	mac profit : 0

	iter #1:
		current profit: -1
		max profit: 0
	iter #2:
		current profit: -2
		max profit: 0
	iter #3:
		current profit: -3
		max profit: 0
	Final - max profit: 0


TC3:
	prices: [2 5 1 7]
	deltas: [3 -4 6]

	current profit: 0
	mac profit : 0
	buy: 2

	iter #1:
		current profit: 3
		max profit: 3
	iter #2:
		current profit: -1
		max profit: 3
		prices[2] (1) < 2
		   buy = 2
		   current profit = 0
	iter #3:
		current profit: 6
		max profit: 6
	Final - max profit: 6
'''



class Solution(object):
	def maxProfit(self, prices):
		"""
		:type prices: List[int]
		:rtype: int
		"""

		if prices == []:
			return 0

		buy = prices[0]
		maxProfit = 0
		currProfit = 0
		for i in range(1, len(prices)):
			currProfit += prices[i] - prices[i-1]
			if currProfit > maxProfit:
				maxProfit = currProfit
			elif prices[i] < buy:
				buy = prices[i]
				currProfit = 0

		return maxProfit



if __name__ == '__main__':
	sol = Solution()

	assert(sol.maxProfit(range(1,8)) == 6)
	assert(sol.maxProfit(range(7, 0, -1)) == 0)
	assert(sol.maxProfit([2,5,1,7]) == 6)
	assert(sol.maxProfit([2,5,1,3]) == 3)
	assert(sol.maxProfit([1,5,2,7]) == 6)
	assert(sol.maxProfit([7,6,5,4,5]) == 1)
	assert(sol.maxProfit([7,6,5,4,6,1,4]) == 3)
	assert(sol.maxProfit([7,2,5,3,6,4,1,7]) == 6)
	assert(sol.maxProfit([7,1,5,3,6,4]) == 5)

	assert(sol.maxProfit([7,6,4,3,1]) == 0)


