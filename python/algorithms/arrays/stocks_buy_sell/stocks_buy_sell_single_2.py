'''
https://leetcode.com/problems/best-time-to-buy-and-sell-stock/

Given an array containing prices of a stock on different days over a period of time, ordered by time.
Calculate when to buy and sell for max profit

At most one transaction is permitted.

NOTE: Can't sell before buy obviously and prices aren't negative


Eg:
	prices: [3 2 4 7 1 5]
	buy: 2, sell: 7, max profit: 5

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


def buy_sell_stock(prices):
	buy_ = prices[0]
	maxProfit = 0
	currProfit = 0
	
	buy = None
	sell = None
	for i in range(1, len(prices)):
		currProfit += prices[i] - prices[i-1]
		if currProfit > maxProfit:
			maxProfit = currProfit
			buy = buy_
			sell = buy_ + maxProfit
		elif prices[i] < buy_:
			buy_ = prices[i]
			currProfit = 0

	return (buy, sell), maxProfit



assert(buy_sell_stock(range(1,8)) == ((1, 7), 6))
assert(buy_sell_stock(range(7, 0, -1)) == ((None, None), 0))
assert(buy_sell_stock([2,5,1,7]) == ((1,7),6))
assert(buy_sell_stock([2,5,1,3]) == ((2,5),3))
assert(buy_sell_stock([1,5,2,7]) == ((1,7),6))
assert(buy_sell_stock([7,6,5,4,5]) == ((4,5),1))
assert(buy_sell_stock([7,6,5,4,6,1,4]) == ((1,4),3))
assert(buy_sell_stock([7,2,5,3,6,4,1,7]) == ((1, 7), 6))
assert(buy_sell_stock([7,1,5,3,6,4]) == ((1, 6), 5))
assert(buy_sell_stock([7,6,4,3,1]) == ((None, None), 0))
assert buy_sell_stock([1,3,2,5]) == ((1, 5), 4)

