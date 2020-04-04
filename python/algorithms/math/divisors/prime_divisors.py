'''
Find all the prime divisors of a number, n
'''
def prime_divisors(n):
	orig_n = n
	divs = []
	i = 2
	while i*i <= orig_n:
		if n % i == 0:
			while n % i == 0:
				n /= i
			divs.append(i)
		i += 1

	return divs


if __name__ == '__main__':
	assert prime_divisors(18) == [2,3]
	assert prime_divisors(2*2*3*3*5*7*7*7*11*12*6) == [2,3,5,7,11]
	assert prime_divisors(49) == [7]
	assert prime_divisors(315) == [3,5,7]
