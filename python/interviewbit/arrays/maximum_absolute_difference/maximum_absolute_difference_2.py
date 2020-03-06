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

'''
Solution Outline: O(n) time
	i. Maximizing a-b (a>=0, b>=0) is the same as maximizing a and minimizing b
	ii. Maximum value of |A[i] - A[j]| + |i-j| depends on
	    A[i] < A[j] and i < j
		  then A[i] - A[j] < 0, i - j < 0
		    max |A[i] - A[j]| + |i-j| would be
			  A[j]-A[i] + j-i = -(A[i] + i) + A[j]+j
			  = (A[j]+j) - (A[i]+i)

	    A[i] > A[j] and i < j
		  then A[i] - A[j] > 0, i - j < 0
		    max |A[i] - A[j]| + |i-j| would be
			  A[i]-A[j] + (j-i) = A[i]-A[j] + j - i
			  = (A[i] - i) - (A[j] - j)

		A[i] < A[j] and i > j
		  then A[i] - A[j] < 0, i - j > 0
		    max |A[i] - A[j]| + |i-j| would be
			  A[j]-A[i] + i-j
			  = (A[j]-j) - (A[i]-i)

		A[i] > A[j] and i > j
		  then A[i] - A[j] < 0, i - j > 0
		    max |A[i] - A[j]| + |i-j| would be
			  A[i] - A[j] + i - j
			  = (A[i] + i) - A[j] +j)

	iii. Find any two pairs (A[i],i) and (A[j],j)
	      s.t. (A[i]+i) - (A[j]+j) is maximized
		 and another set of pairs 
		   s.t. (A[i]-i) - (A[j]-j) is maximized
		 Return the maximum amongst these two pairs.
'''
def find_max_absolute_difference(A):
	min1, max1 = None, None
	min2, max2 = None, None
	for i in xrange(len(A)):
		x = A[i]+i
		y = A[i]-i
		if min1 is None or x < min1:
			min1 = x
		if max1 is None or x > max1:
			max1 = x
		if min2 is None or y < min2:
			min2 = y
		if max2 is None or y > max2:
			max2 = y

	return max(max1-min1, max2-min2)


if __name__ == '__main__':
	assert find_max_absolute_difference([1, 3, -1]) == 5
	assert find_max_absolute_difference([5,4,3,2,1]) == 8
	assert find_max_absolute_difference([1,2,3,4,5]) == 8
	assert find_max_absolute_difference([1,5,2,3,4]) == 7
	assert find_max_absolute_difference([5,1,2,3,4]) == 6

