'''
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
'''

def count_3s(n):
	sum3 = 0
	i = 3
	while i < n:
		sum3 = sum3 + i
		i = i + 3
	
	return sum3


def count_5s_not_3s(n):
	sum5 = 0
	i = 5
	while i < n:
		if (i % 3 != 0):
			sum5 = sum5 + i
		i = i + 5

	return sum5

print count_3s(1000) + count_5s_not_3s(1000)

# soln: 233168
