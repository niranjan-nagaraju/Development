'''
https://www.hackerrank.com/contests/sears-dots-arrows/challenges/the-easy-puzzle-1

Given an array of integers N and a prime number K, your task is to find out if LCM of the whole array is divisible by K. Output YES in case it is divisible, NO otherwise.

Input Format

First line of the input contains an integer T denoting the number of testcases.
First line of each testcase contains two space separated integers N and K.
Second line of each testcase contains N integers separated by a single space.


Output Format
For each of the testcase, print YES or NO in a separate line.

Sample Input
1
3 3
2 3 12

Sample Output
YES

'''

# LCM of two numbers a and b
# lcm(a,b) * gcd(a,b) == a * b
def lcm(a, b):
	return (a * b) / gcd(a,b)


def gcd (a, b):
	if (b == 0):
		return a

	return gcd(b, a % b)

t = int(input())

for i in xrange(t):
	n, k = map(int, raw_input().split())
	arr = map(int, raw_input().split())
	curr_lcm = 1
	for x in arr:
		curr_lcm = lcm(curr_lcm, x)

	print 'YES' if (curr_lcm % k == 0) else 'NO'

