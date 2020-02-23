'''
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
'''


# sigma(3, ... 999) + sigma(5, ... 995) - sigma(15, ... 1000/15)

def sigma(n):
	return n*(n+1)/2


# 995/5 === 999/5, 999/15 === 999/15
print 3*sigma(999/3) + 5*sigma(999/5) - 15*sigma(999/15)

# soln: 233168
