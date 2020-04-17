'''
https://www.interviewbit.com/problems/square-root-of-integer

Given an integar A.

Compute and return the square root of A.

If A is not a perfect square, return floor(sqrt(A)).

DO NOT USE SQRT FUNCTION FROM STANDARD LIBRARY



Input Format

The first and only argument given is the integer A.
Output Format

Return floor(sqrt(A))
Constraints

1 <= A <= 10^9
For Example

Input 1:
    A = 11
Output 1:
    3

Input 2:
    A = 9
Output 2:
    3
'''

'''
Solution Outline:
	Use binary search to zero in on square(i) == n
	Initial window: l=0, r=n
	i = (l+r)/2
	if square(i) == n, return i as sqrt
	if square(i) < n, window = (i+1, r)
	if square(i) > n, window = (l, i-1)
'''

class Solution:
	def sqrt(self, n):
		l = 0
		r = n
		res = 0
		while l <= r:
			mid = (l+r)/2

			if mid*mid == n:
				return mid

			if mid*mid < n:
				# Use the last 'mid' s.t. mid**2 < n
				# before the left and right of the window cross
				# each other
				res = mid
				l = mid+1
			else: # mid*mid > n
				r = mid-1

		return res

if __name__ == '__main__':
	s = Solution()
	assert s.sqrt(0) == 0
	assert s.sqrt(1) == 1
	assert s.sqrt(4) == 2
	assert s.sqrt(6) == 2
	assert s.sqrt(9) == 3
	assert s.sqrt(10) == 3
	assert s.sqrt(18) == 4
	assert s.sqrt(20) == 4
	assert s.sqrt(100) == 10

