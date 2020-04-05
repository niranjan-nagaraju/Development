#encoding: utf-8
'''
https://www.interviewbit.com/problems/n3-repeat-number/

N/3 Repeat Number

You’re given a read only array of n integers. Find out if any integer occurs more than n/3 times in the array in linear time and constant additional space.
If so, return the integer. If not, return -1.
If there are multiple solutions, return any one.

Example :
	Input : [1 2 3 1 1]
	Output : 1 
	1 occurs 3 times which is more than 5/3 times.
'''

'''
Solution Outline:
	Use a variation of Moore's voting algorithm, but select 2 candidates,
	and at the end of the first pass, verify the frequency of either candidates
	to see which of them is > n/3

Sample run:
	A: [3, 2, 3, 2, 3, 4, 1]

	count1 = 0
	count2 = 0

	x: 3
	  candidate1 = 3
	  count1 = 1

	x: 2
	   candidate2 = 2
	   count2 = 1

	x: 3 == candidate1
	  count1 = 2

	x: 2 == candidate2
	  count2 = 2

	x: 3 == candidate1
	  count1 = 3

	x: 4 != candidate1, candidate2
	  count1 = 2
	  count2 = 1

	x: 1 != candidate1, candidate2
	  count1 = 1
	  count2 = 0

	freq(candidate1) = 3 > 6/3 => return 3


Sample run 2:
	A: [1, 2, 3, 1, 2, 3, 3]
	
	count1 = 0
	count2 = 0

	x: 1
	  candidate1 = 1
	  count1 = 1
	
	x: 2
	  candidate2 = 2
	  count2 = 1

	x: 3 != candidate1, candidate2
	  count1 = 0
	  count2 = 0

	x: 1 == candidate1, candidate2
	  count1 == 1

	x: 2 == candidate2
	  count2 = 1

	x: 3 != candidate1, candidate2
	  count1 = 0
	  count2 = 0

	x: 3 != candidate1, candidate2
	  count1 == 0
	    candidate1 = 3
		count1 = 1

	candidate1, candidate2 = 3, 2
	freq(candidate1) = 3 > 6/3 => return 3


Sample run 3:
	A: [1,2,3,1,2,1,2]

	count1 = count2 = 0
	candidate1 = candidate2 = None

	x: 1
	candidate1 = 1, count1 = 1

	x: 2
	candidate2 = 2, count2 = 1

	x: 3 != candidate1, candidate2
	count1 = 0
	count2 = 0

	x: 1
	  count1 == 0
	  candidate1 = 1, count1 = 1

	x: 2 == candidate2
	  count2 = 1

	x: 1 == candidate1
	  count1 = 2
	
	x: 2 == candidate2
	  count2 = 2

	
	freq(1) = 3 > 7/3 
	freq(2) = 3 > 7/3
'''
class Solution:
	def n3_repeat(self, A):
		n = len(A)
		
		candidate1 = candidate2 = None
		count1 = count2 = 0
		for x in A:
			if x == candidate1:
				count1 += 1
			elif x == candidate2:
				count2 += 1
			elif count1 == 0:
				candidate1 = x
				count1 = 1
			elif count2 == 0:
				candidate2 = x
				count2 = 1
			else:
				count1 -= 1
				count2 -= 1

		count1 = count2 = 0
		for x in A:
			if x == candidate1:
				count1 += 1
				if count1 > n/3:
					return candidate1
			elif x == candidate2:
				count2 += 1
				if count2 > n/3:
					return candidate2

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.n3_repeat([1]) == 1
	assert s.n3_repeat([1,2,3,1,1,1]) == 1
	assert s.n3_repeat([3,2,3,2,3,4,1]) == 3
	assert s.n3_repeat([1,2,3,1,2,3,3]) == 3
	assert s.n3_repeat([1,2,3,1,2,1,2]) == 1
	assert s.n3_repeat([ 1, 1, 1, 2, 3, 5, 7 ]) == 1

