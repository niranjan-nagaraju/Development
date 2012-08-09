# Given last month's day-to-day prices of a stock
# and that this month's statistics will exactly follow last month's
# Suggest when to buy and when to sell (obviously with profit maximized)
# Source: Oracle

# Return time from the (price, time) pair
def time(price_time_pair):
	return price_time_pair[1]


# Return price from the (price, time) pair
def price (price_time_pair):
	return price_time_pair[0]


# If buy is at [index], find optimal sell 'after' we buy
# Start from rightmost (maximum price ever) and slide towards left
# until we can make a sell (i.e if we can buy first, then sell)
def find_optimal_sell_for_current_price (prices_list, i):
	buy = prices_list[i]
	j = len(prices_list)-1

	while ( i < j ):
		sell = prices_list[j]
		if (time(buy) < time(sell)):
			return sell
		j = j-1

	return (0,0)


# Optimal buy and sell is calculated by
# starting at index 0, calculating its sell, thereby the resulting profit
# and then at index 1, its sell price and profit
# Return one that gives maximum profit
def find_optimal_buy_and_sell (prices_list):
	# Create (price, time) pairs and order them by price
	prices_list = zip (prices_list, range(1, len(prices_list)+1))
	prices_list.sort()

	curr_max_profit = 0
	(profit_buy, profit_sell) = (0,0)
	for i in range(0, len(prices_list)-1):
		buy = prices_list[i]
		sell = find_optimal_sell_for_current_price(prices_list, i)

		# Current sell and buy prices yields more profit
		# than we have seen thus far
		if ( (price(sell) - price(buy)) > curr_max_profit ):
			curr_max_profit = price(sell) - price(buy)
			(profit_buy, profit_sell) = (buy, sell)
	
	return (profit_buy, profit_sell)


prices_list_by_days = [3, 4, 7, 2, 8, 1, 5]
print find_optimal_buy_and_sell(prices_list_by_days)

''' 
 Trial runs

prices_list_by_days = [3, 4, 7, 2, 8, 1, 5]
((2, 4), (8, 5))
prices_list_by_days = [21,1,5,22,1,9,6,24,26]
((1, 2), (26, 9))

'''


