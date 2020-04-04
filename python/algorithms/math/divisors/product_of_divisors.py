'''
Product of divisors of a number

product_of_divisors(18) = 1*2*3*6*9*18 = 324

product_of_divisors(n) = power(n, d/2), where d is the number of divisors
'''

from algorithms.math.divisors.divisors import count_divisors, divisors
from algorithms.math.power.power import power
from math import sqrt

def product_of_divisors(n):
	num_d = count_divisors(n)

	prod = power(n, num_d/2)

	# if num_d is odd, then we'll have a 0.5 remaining after num_d/2
	# multiply product by sqrt(n) to get power(n, num_d/2.0)
	if num_d & 1 == 1:
		prod *= sqrt(n)

	return prod


# Multiply all numbers in the list
def product(lst):
	return reduce(lambda x,y: x*y, lst, 1)

if __name__ == '__main__':
	assert (product_of_divisors(18) == product(divisors(18)) == 5832)
	assert (product_of_divisors(49) == product(divisors(49)) == 343)
