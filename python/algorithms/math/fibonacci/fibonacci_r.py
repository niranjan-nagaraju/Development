'''
Calculate nth fibonacci number

fib(1) = 1
fib(2) = 1
fib(n) = fib(n-1) + fib(n-2)
'''

'''
Recursive version
  Inefficient: because we might be recomputing an ith fibonacci value multipe times

fibonacci(6) = fibonacci(5) + fibonacci(4)
	= (fibonacci(4) + fibonacci(3)) + (fibonacci(3) + fibonacci(2))
	= ((fibonacci(3) + fibonacci(2)) + (fibonacci(2) + fibonacci(1))) + ((fibonacci(2) + fibonacci(1)) + 1)
	= (((fibonacci(2) + fibonacci(1)) + 1) + (1 + 1)) + ((1 + 1) + 1)
	= (((1 + 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
	= ((2 + 1) + (1 + 1)) + ((1 + 1) + 1)
	= (3 + (1 + 1)) + ((1 + 1) + 1)
	= (3 + 2) + ((1 + 1) + 1)
	= (3 + 2) + (2 + 1)
	= (5) + (2 + 1)
	= (5) + (3)
	= 8
'''
def fibonacci_r(n):
	if n < 3:
		return 1

	return fibonacci_r(n-1) + fibonacci_r(n-2)



'''
Tail-recursive version
fibonacci(n) = go(n, 1, 1)
go(1, a, b) = a
go(2, a, b) = b
go(n, a, b) = go(n-1, b, a+b)

fibonacci(6):
	go(6, 1, 1)
	= go(5, 1, 2)
	= go(4, 2, 3)
	= go(3, 3, 5)
	= go(2, 5, 8)
	= 8
'''
def fibonacci_tr(n):
	def go(n, a, b):
		if n == 1:
			return a
		if n == 2:
			return b

		return go(n-1, b, a+b)

	return go(n, 1, 1)


if __name__ == '__main__':
	assert fibonacci_r(1) == 1
	assert fibonacci_r(2) == 1
	assert fibonacci_r(3) == 2
	assert fibonacci_r(4) == 3
	assert fibonacci_r(5) == 5
	assert fibonacci_r(6) == 8
	assert fibonacci_r(7) == 13
	assert fibonacci_r(8) == 21
	assert fibonacci_r(9) == 34
	assert fibonacci_r(10) == 55

	assert fibonacci_tr(1) == 1
	assert fibonacci_tr(2) == 1
	assert fibonacci_tr(3) == 2
	assert fibonacci_tr(4) == 3
	assert fibonacci_tr(5) == 5
	assert fibonacci_tr(6) == 8
	assert fibonacci_tr(7) == 13
	assert fibonacci_tr(8) == 21
	assert fibonacci_tr(9) == 34
	assert fibonacci_tr(10) == 55

