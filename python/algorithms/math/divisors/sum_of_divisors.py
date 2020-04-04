'''
n = p1 ** a1 * p2 ** a2 ...
then
sum_of_divisors_including_n = (1+ p1 + p1**2 + .. + p1**a1) * (1+ p2 + .. p2**a2) * ...
'''

from algorithms.math.divisors.divisors import divisors
def sum_of_divisors(n):
	prod = 1
	k = 2
	while (k*k <= n):
		p = 1
		while (n % k == 0):
			p = p * k + 1
			n /= k
		prod *= p
		k += 1

	if n > 1:
		prod *= 1+n

	return prod


if __name__ == '__main__':
	assert (sum_of_divisors(24) == sum(divisors(24)) == 60)
	assert (sum_of_divisors(18) == sum(divisors(18)) == 39)

