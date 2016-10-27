'''
https://www.hackerrank.com/challenges/fibonacci-modified

t{i+2} = ti + t{i+1}**2

Given t0, t1, calculate tn

Input:
	t0, t1, 3<=n<=20

Sample Input
0 1 5

Sample Output
5
'''

def modified_fib (t0, t1, n):
	mfibs = [0] * 20
	mfibs[0], mfibs[1] = t0, t1

	i = 2
	while i < n:
		mfibs[i] = mfibs[i-2] + mfibs[i-1]**2
		i += 1

	return mfibs[n-1]


t0, t1, n = map(int, raw_input().split())
print modified_fib(t0, t1, n)
