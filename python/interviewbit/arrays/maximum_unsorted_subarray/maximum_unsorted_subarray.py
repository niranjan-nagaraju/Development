#encoding: utf-8
'''
https://www.interviewbit.com/problems/maximum-unsorted-subarray/

Maximum Unsorted Subarray

You are given an array (zero indexed) of N non-negative integers, A0, A1 ,…, AN-1.
Find the minimum sub array Al, Al+1 ,…, Ar so if we sort(in ascending order) that sub array, then the whole array should get sorted.
If A is already sorted, output -1.

Example :
	Input 1:
	A = [1, 3, 2, 4, 5]
	Return: [1, 2]

	Input 2:
	A = [1, 2, 3, 4, 5]
	Return: [-1]

In the above example(Input 1), if we sort the subarray A1, A2, then whole array A should get sorted.
'''

'''
Solution Outline:
	1. Scan Left-right and stop at index, 'l' where a[l] > a[l+1]
	2. Scan right-left and stop at index, 'r' where a[r] < a[r-1]
	3. a[l..r] is unsorted,
	   However, this might not be the maximum unsorted subarray => sorting a[l..r] might not sort the whole array.
	   e.g.,
	       0 1 2 3 4 5 6 7
	   A: [1,3,4,5,2,6,8,9]
	   l: 3, a[l] = 5
	   r: 4, a[r] = 2
	   However, sorting a[l..r] = [5,2] = [2,5]
	   wont sort the whole array
	   A: [1,3,4,2,5,6,8,9] is not sorted.
	4. To find the maximum unsorted array, expand the scope of l..r
	   let min, max be the maximum and minimum within a[l..r]
	   Scan L-R to find the first element in a[0..l-1], a[i], that's greater than a[l], =>
	   i is the right place for a[l], replace  l = i
	   Similarly, If scan the array R-L to find the first element in a[r+1..n-1], a[j] < a[r], then replace that as r
	   Return [l..r]

	   In the above example,
	    a[l..r] = [5,2]
		min: 2, max: 5
		 i=1, a[i]=3 > min => l = 1
		 j=None, as all of a[5..7] > a[r] => no change in r

		return (1,4)

		A[1..4] == [4,5,2] => sort: [2,4,5]
		A: [1,2,4,5,6,8,9]

Sample run:
	A: [10, 12, 20, 30, 25, 40, 32, 31, 35, 50, 60]
        0   1   2   3   4   5   6   7   8   9   10
	l: 3 A[l] > A[l+1]
	r: 7 A[r] < A[r-1]
	A[l..r] = A[3..7] = [30,25,40,32,31]
	min: 25, max: 40
	Position for min in A[0..l-1] == 3 => l = 3
	Position for max in A[r+1..n-1] == A[8..10] = 8 => r = 8

	return (3,8)
	A[3..8] = [30, 25, 40, 32, 31, 35] => sort() => [25,30,31,32,35,40]
	A: [10, 12, 20, 25, 30, 31, 32, 35, 40, 50, 60]
'''
class Solution:
	def maxUnsorted(self, A):
		if not A:
			return [-1]

		n = len(A)
		l = 0
		while l<n-1 and A[l] <= A[l+1]:
			l += 1

		# Entire list is already sorted
		if l == n-1:
			return [-1]

		r = n-1
		while r > 0 and A[r] >= A[r-1]:
			r -= 1

		minimum = A[l]
		maximum = A[r]
		for i in xrange(l, r+1):
			if A[i] < minimum:
				minimum = A[i]
			elif A[i] > maximum:
				maximum = A[i]

		for i in xrange(l):
			if A[i] > minimum:
				l = i
				break
			i += 1

		for j in xrange(n-1, r, -1):
			if A[j] < maximum:
				r = j
				break
			j -= 1

		return [l,r]



if __name__ == '__main__':
	s = Solution()
	assert s.maxUnsorted([]) == [-1]
	assert s.maxUnsorted([6]) == [-1]
	assert s.maxUnsorted([1,2,3,4,5]) == [-1]
	assert s.maxUnsorted([1,3,2,4,5]) == [1,2]
	assert s.maxUnsorted([5,4,3,2,1]) == [0,4]
	assert s.maxUnsorted([1,3,4,5,2,6,8,9]) == [1,4]
	assert s.maxUnsorted([10, 12, 20, 30, 25, 40, 32, 31, 35, 50, 60]) == [3,8]
	assert s.maxUnsorted([10, 12, 20, 30, 9, 40, 32, 31, 35, 50, 60]) == [0,8]
	assert s.maxUnsorted([10, 12, 20, 30, 40, 31, 32, 35, 50, 60]) == [4,7]
	assert s.maxUnsorted([0, 1, 15, 25, 6, 7, 30, 40, 50]) == [2,5]

