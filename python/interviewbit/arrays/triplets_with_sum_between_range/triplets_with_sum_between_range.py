#encoding: utf-8

'''
https://www.interviewbit.com/problems/triplets-with-sum-between-given-range/

Triplets with Sum between given range

Given an array of real numbers greater than zero in form of strings.
Find if there exists a triplet (a,b,c) such that 1 < a+b+c < 2 .
Return 1 for true or 0 for false.

Example:

Given [0.6, 0.7, 0.8, 1.2, 0.4] ,
You should return 1
as
0.6+0.7+0.4=1.7
1<1.7<2
Hence, the output is 1.

O(n) solution is expected.
Note: You can assume the numbers in strings don’t overflow the primitive data type and there are no leading zeroes in numbers.
Extra memory usage is allowed.
'''

'''
Solution Outline:
	1. Sort the array
	2. Fix an i, 0..n-1
		Use 2 pointers, l:i+1, r: n-1
		 sum:= a[i]+j[l]+a[r]
		if sum < 1, move l,
		if sum > 2, move r
		if 1 < sum < 2, return true
'''
class Solution:
	def tripletsSum(self, A):
		n = len(A)
		if n < 3:
			return 0

		A.sort(cmp=lambda x, y: cmp(float(x), float(y)))

		for i in xrange(n):
			l,r = i+1,n-1
			while l<r:
				curr_sum = float(A[i])+float(A[l])+float(A[r])
				if 1 < curr_sum < 2:
					return 1
				if curr_sum <= 1:
					l += 1
				else: # curr_sum >= 2
					r -= 1
				
		return 0


if __name__ == '__main__':
	s = Solution()
	assert s.tripletsSum(["0.1", "0.8", "0.6", "0.3", "0.9"]) == 1
	assert s.tripletsSum([ "0.366507", "0.234601", "2.126313", "1.372403", "2.022170", "0.308558", "2.120754", "1.561462" ]) == 1
	assert s.tripletsSum(["0.1", "0.8", "0.6", "0.3", "0.9"]) == 1
	assert s.tripletsSum(["0.6", "0.7", "0.8", "1.2", "0.4"]) == 1
	assert s.tripletsSum([ "2.673662", "2.419159", "0.573816", "2.454376", "0.403605", "2.503658", "0.806191" ]) == 1

