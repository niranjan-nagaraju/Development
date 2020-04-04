'''
Find list of divisors for a given number, or a count of number of divisors a number has
'''


# Brute force, Count how many of [1,2,..,n] divides n
# and return both the count and the divisors list
def divisors_1(n):
	divs = [1]
	for i in range(2, n/2+1):
		if ( n % i == 0):
			divs.append(i)

	divs.append(n)
	return divs



# Faster-approach, All divisors, a, below sqrt(n)
# has its pair, b, on the other side of sqrt(n)
# i.e., a*b == n, then a < sqrt(n) and b > sqrt(n) { except when a == b == sqrt(n)}
# So include all divisors < sqrt(n) + its pair (i, n/i) {+ sqrt(n) if sqrt(n) is an integer}
def divisors(n):
	divs_1 = []
	divs_2 = []
	i = 1
	while i * i <= n:
		if n % i == 0:
			# If n is a square number
			# include/count its square root only once
			if n / i == i:
				divs_1.append(i)
			else:
				divs_1.append(i)
				divs_2.insert(0, n/i)
		i += 1

	divs = divs_1 + divs_2
	return divs


# Faster-approach, All divisors, a, below sqrt(n)
# has its pair, b, on the other side of sqrt(n)
# i.e., a*b == n, then a < sqrt(n) and b > sqrt(n) { except when a == b == sqrt(n)}
# So count all divisors < sqrt(n) twice {+ one if sqrt(n) is an integer}
def count_divisors(n):
	num_d = 0
	i = 1
	while i * i <= n:
		if n % i == 0:
			# If n is a square number
			# include/count its square root only once
			if n / i == i:
				num_d += 1
			else:
				num_d += 2
		i += 1

	return num_d


if __name__ == '__main__':
	assert divisors_1(18) == [1,2,3,6,9,18]
	assert divisors(18) == [1,2,3,6,9,18]
	assert count_divisors(18) == 6

	assert divisors_1(49) == [1,7,49]
	assert divisors(49) == [1,7,49]
	assert count_divisors(49) == 3

