'''
https://www.hackerrank.com/challenges/find-digits

Number of digit of N that evenly divide N

Sample Input
2
12
1012

Sample Output
2
3

'''

def find_num_digits_dividing(N):
	num_of_digits = 0
	rest_of_n = N

	while rest_of_n != 0:
		digit = rest_of_n % 10
		rest_of_n = rest_of_n / 10

		try:
			if (N % digit == 0):
				num_of_digits += 1
		except ZeroDivisionError:
			continue


	return num_of_digits



n = int(raw_input())
for i in xrange(n):
	N = int(raw_input())
	print find_num_digits_dividing(N)

