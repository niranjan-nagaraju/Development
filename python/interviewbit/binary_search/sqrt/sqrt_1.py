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
Solution Outline: Brute force, O(n) time
	Start with candidate = n/2
	Decrement candidate by 1 until its square <= n
	return candidate as the square root
'''

class Solution:
	def sqrt(self, n):
		if n == 1:
			return 1

		i = n/2
		while i*i > n:
			i -= 1

		return i


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

