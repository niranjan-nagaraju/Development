'''
Generate first n fibonacci numbers
'''
def fibonacci_generator(n):
	a = b = 1
	while n > 0:
		yield a
		a, b = b, a+b
		n -= 1


if __name__ == '__main__':
	assert [_ for _ in fibonacci_generator(5)] == [1,1,2,3,5]
	assert [_ for _ in fibonacci_generator(10)] == [1,1,2,3,5,8,13,21,34,55]

