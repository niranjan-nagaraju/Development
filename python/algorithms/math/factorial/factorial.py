'''
Compute factorial of a number

factorial(n) = n * (n-1) * (n-2) * ... * 3 * 2 * 1
factorial(1) = 1
factorial(0) = 1
'''

'''
Recursive solution:
	factorial(n) = n * factorial(n-1)
	factorial(1) = 1
	factorial(0) = 1


factorial(4):
	= 4 * factorial(3)
	= 4 * (3 * factorial(2))
	= 4 * (3 * (2 * factorial(1)))
	= 4 * (3 * (2 * 1))
	= 4 * (3 * 2)
	= 4 * 6
	= 24
'''
def factorial_r(n):
	if n <= 1:
		return 1

	return n * factorial_r(n-1)



'''
Tail-recursive version for better reduction and unfolding
	factorial(n) = go(n, 1)
	go(n, acc) = go((n-1), n*acc)
	go(1, acc) = acc

factorial(4):
	= go(4, 1)
	= go(3, 4)
	= go(2, 12)
	= go(1, 24)
	= 24
'''
def factorial_tr(n):
	# tail-recursive factorial
	def go(n, acc):
		# End of recursion, return accumulated product so-far
		if n == 1:
			return acc

		return go((n-1), n*acc)

	if n <= 1:
		return 1

	return go(n, 1)


if __name__ == '__main__':
	assert factorial_r(4) == 24
	assert factorial_r(0) == 1

	assert factorial_tr(4) == 24
	assert factorial_tr(0) == 1

