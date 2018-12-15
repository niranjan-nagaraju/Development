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
	Start with (max)profit = {0,0,0}
	Tentative buy_ = prices[0], Day 1
	For each subsequent day i, 
	  buy_ can be replaced by another x = prices[i] if x < buy_

	  However, as soon as we find a potential sell at x = prices[i], where current profit (x-buy_) > max profit
	     capture (buy, sell, profit) as (buy_, prices[i] and profit_)
	  Continue to replace buy_ if x < buy_, or replace entire (buy, sell, profit) if we found better (buy, sell) prices.


TC1:
	prices: [1 2 3 4]
	buy_: 1, profit: 0
	iter #1:
		buy_ = 1, sell_ = 2, profit_ = 1
		buy, sell, profit: 1, 2, 1
	iter #2:
		buy_ = 1, sell_ = 3, profit_ = 2
		buy, sell, profit: 1, 3, 2
	iter #3:
		buy_ = 1, sell_ = 4, profit_ = 3
		buy, sell, profit: 1, 4, 3
	final - buy, sell, profit: 1, 4, 3

TC2:
	prices: [4 3 2 1]
	buy_: 4, profit: 0
	iter #1:
		buy_ = 3, sell_ = , profit_ = 
	iter #2:
		buy_ = 2, sell_ = , profit_ = 
	iter #3:
		buy_ = 1, sell_ = , profit_ = 
	final - buy, sell, profit: None, None, None

TC3:
	prices: [2 5 1 7]
	buy_: 2, profit: 0
	iter #1:
		buy_ = 2, sell_ = 5, profit_ = 3
		buy, sell, profit: 2, 5, 3
	iter #2:
		buy_ = 1, sell_ = , profit_ =
	iter #3:
		buy_ = 1, sell_ = 7, profit_ = 6 > profit (3)
		buy, sell, profit: 1, 7, 6
	final - buy, sell, profit: 1, 7, 6

TC4:
	prices: [2 5 1 3]
	buy_: 1, profit: 0
	iter #1:
		buy_ = 1, sell_ = 5, profit_ = 3
		buy, sell, profit: 2, 5, 3
	iter #2:
		buy_ = 1, sell_ = , profit_ =
	iter #3:
		buy_ = 1, sell_ = 3, profit_ = 2 < profit (3)
	final - buy, sell, profit: 2, 5, 3

TC5:
	prices: [1 5 2 7]
	buy_: 1, profit: 0
	iter #1:
		buy_ = 1, sell_ = 5, profit_ = 4
		buy, sell, profit: 1, 5, 4
	iter #2:
		buy_ = 1, sell_ = , profit_ = 
	iter #1:
		buy_ = 1, sell_ = 7, profit_ = 6
		buy, sell, profit: 1, 7, 6
	final - buy, sell, profit: 1, 7, 6
'''

def buy_sell_stock(prices):
	buy, sell, profit = None, None, 0

	buy_ = prices[0]
	for x in prices[1:]:
		if x < buy_:
			buy_ = x
		else:
			profit_ = x - buy_
			if profit_ > profit:
				buy, sell, profit = buy_, x, profit_
				#print 'Tentatives:', buy, sell, profit

	return (buy, sell, profit)


assert(buy_sell_stock(range(1,8)) == (1, 7, 6))
assert(buy_sell_stock(range(7, 0, -1)) == (None, None, 0))
assert(buy_sell_stock([2,5,1,7]) == (1,7,6))
assert(buy_sell_stock([2,5,1,3]) == (2,5,3))
assert(buy_sell_stock([1,5,2,7]) == (1,7,6))
assert(buy_sell_stock([7,6,5,4,5]) == (4,5,1))
assert(buy_sell_stock([7,6,5,4,6,1,4]) == (1,4,3))
assert(buy_sell_stock([7,2,5,3,6,4,1,7]) == (1, 7, 6))
assert(buy_sell_stock([7,1,5,3,6,4]) == (1, 6, 5))
assert(buy_sell_stock([7,6,4,3,1]) == (None, None, 0))
