'''
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

	1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
'''


# f2 + f5 + f8 + ...
# f1 = x, f2 = y
# x, y, (x+y), (x+2y), (2x+3y)
# 1, [2], 3, (5,  [8])
# 5, 8, 13,  (21, [34])
# 21, 34, 55, (89, [144])
def sum_even_fibs():
	x = 1
	y = 2
	
	sum_fibs = 2
	while True:
		a = x + 2*y
		y = 2*x + 3*y
		x = a

		if (y > 4000000):
			print sum_fibs
			return

		sum_fibs = sum_fibs + y

	print sum_fibs	
			

sum_even_fibs()

# soln: 4613732
