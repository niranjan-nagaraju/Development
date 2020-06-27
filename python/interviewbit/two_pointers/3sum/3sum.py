'''
https://www.interviewbit.com/problems/3-sum/

3 Sum

Given an array S of n integers, find three integers in S such that the sum is closest to a given number, target.
Return the sum of the three integers.

Assume that there will only be one solution

Example:
given array S = {-1 2 1 -4},
and target = 1.

The sum that is closest to the target is 2. (-1 + 2 + 1 = 2)
'''

'''
Solution Outline:
	0. Let S be the array of integers, and target = target sum
	1. Sort the array S
	2. Start with a,b,c = S[i], S[i+1], and S[n-1], i = 0
	   2.1 Calculate curr_sum := (a+b+c)
	       Check how close is it to target, and initialize delta = |curr_sum - target|
	   2.2 If curr_sum is closer to target than previous closest sum, (|curr_sum - target| < delta)
	         Update closest sum to curr_sum, delta to |curr_sum - target|
	   2.3 If curr_sum is greater than target, Move c towards left to reduce curr_sum
		   If curr_sum is lesser than target, Move b towards right to increase curr_sum
		   If curr_sum == target => we have found an exact match, can't get closer to our target sum than this
		     return curr_sum
	   2.4 If b & c cross each other, Start 2. over with i=1
		     a,b,c = S[i], S[i+1], S[-1]


Sample run:
	S: [1,4,-5,8,11], target=12

	sort(S) => [-5,1,4,8,19]

	a = -5, b = 1, c = 19
	curr_sum = 15 > target
	  c = 8
	  delta = |12-15| = 3
	  closest sum = (-5. 1. 19) == 15

	a = -5, b = 1, c = 8
	curr_sum = 4 < target
	  b = 4
	  delta = |12-4| = 8 (> delta seen so far)

	a = -5, b = 4, c = 8
	curr_sum = 7 < target
	  b = 8
	  delta = |12-7| = 5 (> delta seen so far)
	  b == c
	
	Start over with a=1, b=4,c=19
	curr_sum = 24 > target
	  c = 8
	  delta = |12-24| = 12 (> delta seen so far)

	a = 1, b = 4, c = 8
	curr_sum = 13 > target
	  c=4
	  delta = |12-13| = 1 (< delta seen so far)
	  closest sum = (1,4,8) == 13

	a = 1, b = 4, c = 4
	  b == c
	
	Start over with a=4, b=8,c=19
	curr_sum = 31 > target
	  c = 8
	  delta = |12-31| = 19 (> delta seen so far)

	a = 4, b = 8, c = 8
	  b == c

	Start over with a=8, b=19,c=??? (END OF ARRAY)
	return 13 (1,4,8) as the closest triplet sum to target (12)
'''
class Solution:
	def find_closest_3sum(self, S, target):
		if not S or len(S) < 3:
			return sum(S)

		S.sort()
		i = 0
		delta = None
		while i < len(S)-2:
			l = i+1
			r = len(S)-1
			while l < r:
				curr_sum = S[i] + S[l] + S[r]
				if delta is None or abs(curr_sum - target) < delta:
					delta = abs(curr_sum-target)
					closest_3sum = curr_sum

				if curr_sum < target:
					l += 1
				elif curr_sum > target:
					r -= 1
				else: # Found an exact match
					return curr_sum
			i += 1

		return closest_3sum


if __name__ == '__main__':
	s = Solution()
	assert s.find_closest_3sum([-1, 2, 1, -4], 1) == 2
	assert s.find_closest_3sum([1,2,3,4,-5], 10) == 9
	assert s.find_closest_3sum([1,4,-5,8,11], 12) == 13

