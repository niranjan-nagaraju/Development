#encoding: utf-8
'''
https://www.interviewbit.com/problems/maximum-absolute-difference/

Maximum Absolute Difference

You are given an array of N integers, A1, A2 ,…, AN. Return maximum value of f(i, j) for all 1 ≤ i, j ≤ N.
f(i, j) is defined as |A[i] - A[j]| + |i - j|, where |x| denotes absolute value of x.

For example,

A=[1, 3, -1]

f(1, 1) = f(2, 2) = f(3, 3) = 0
f(1, 2) = f(2, 1) = |1 - 3| + |1 - 2| = 3
f(1, 3) = f(3, 1) = |1 - (-1)| + |1 - 3| = 4
f(2, 3) = f(3, 2) = |3 - (-1)| + |2 - 3| = 5

So, we return 5.
'''
def find_max_absolute_difference(A):
	max_abs_diff = 0
	for i in xrange(len(A)):
		for j in xrange(i+1, len(A)):
			curr = abs(A[i] - A[j])  + abs(i-j)
			if curr > max_abs_diff:
				max_abs_diff = curr

	return max_abs_diff


if __name__ == '__main__':
	assert find_max_absolute_difference([1, 3, -1]) == 5
	assert find_max_absolute_difference([5,4,3,2,1]) == 8
	assert find_max_absolute_difference([1,2,3,4,5]) == 8
	assert find_max_absolute_difference([1,5,2,3,4]) == 7
	assert find_max_absolute_difference([5,1,2,3,4]) == 6

