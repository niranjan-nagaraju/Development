# Given last month's day-to-day prices of a stock
# and that this month's statistics will exactly follow last month's
# Suggest when to buy and when to sell (obviously with profit maximized)
# sell and buy ONCE

# Stackoverflow solution to maximize profit
# http://stackoverflow.com/questions/1663545/find-buy-sell-prices-in-array-of-stock-values-to-maximize-positive-difference

def find_optimal_buy_and_sell (priceList):
	max = 0
	maxDiff = 0

	# when a new bottom is hit, it could potentially be a buy
	# unless we don't get a profit margin bigger than previously seen
	bottom = priceList[0]
	diff = 0

	i = 1

	# Calculate consecutive differences
	# keep accumulating as long as the profit margin keeps on increasing
	while (i < len(priceList)):
		diff += priceList[i] - priceList[i-1]

		# Update max profit margin
		if diff > maxDiff:
			maxDiff = diff
			max = priceList[i]

		# We have hit a new bottom
		# record it and reset profit margin to 0
		# See if buying at this price can yield a better margin
		if priceList[i] < bottom:
			bottom = priceList[i]
			diff = 0
		
		i += 1


	print "buy at ", (max-maxDiff), "Sell at", max


priceList = map(int, raw_input().split()) # [3,4,5,8,7,1]
print priceList

find_optimal_buy_and_sell(priceList)


