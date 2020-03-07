'''
https://www.interviewbit.com/problems/noble-integer/

Noble Integer
Given an integer array, find if an integer p exists in the array such that the number of integers greater than p in the array equals to p
If such an integer is found return 1 else return -1.
'''

'''
Solution Outline: O(nlogn) time, O(1) memory
	1. Sort the input array.
	2. Since the array can contain repetitions, For each x = a[i] 0<=i<n,
	    ignore repetitions of x, a[i..j-1], a[j] != x
		Number of integers greater than a[i] would be n-j-1
		check if a[i] == n-j-1

Sample run:
	A: [2,3,2,1,4]
	sorted A: [1, 2, 2, 3, 4]
	n = 5

	i: 0, a[i] = 1
	num_greater = n-i-1 = 5-0-1 = 4 != a[i]

	i: 1, a[i] = 2
	ignore a[i] == 2
	  i = 2, a[i] == 2
	  i = 3, a[i] != 2
	  i = 2
	  num_greater = n-i-1 = 5-2-1 = 2 == a[i]
	  return 1
'''
class Solution:
	def is_there_noble_integer(self, A):
		n = len(A)
		A.sort()

		i = 0
		while i < n:
			x = A[i]
			while i < n and A[i] == x:
				i += 1
			i -= 1
			num_greater = n-i-1
			if num_greater == x:
				#print x
				return 1
			i += 1

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.is_there_noble_integer([2,3,2,1,4]) == 1
	assert s.is_there_noble_integer([5,1,2,3,4]) == -1
	assert s.is_there_noble_integer([6,5,1,2,3,4]) == 1
	assert s.is_there_noble_integer([5,5,1,2,3,6]) == 1
	assert s.is_there_noble_integer([4,6,1,1,3,6,3]) == 1

