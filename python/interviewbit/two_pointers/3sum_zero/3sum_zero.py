#encoding: utf-8
'''
https://www.interviewbit.com/problems/3-sum-zero/

3 Sum Zero

Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0?
Find all unique triplets in the array which gives the sum of zero.

Note:
	Elements in a triplet (a,b,c) must be in non-descending order. (ie, a ≤ b ≤ c)
	The solution set must not contain duplicate triplets.
	For example, given array S = {-1 0 1 2 -1 -4}, A solution set is:
	(-1, 0, 1)
	(-1, -1, 2) 
'''

'''
Solution Outline:
	1. Sort the input array S
	2. Start with a,b,c = S[i], S[i+1], and S[n-1], i = 0
	   2.1 Calculate curr_sum := (a+b+c)
	   2.2 If curr_sum is greater than 0, Move c towards left to reduce curr_sum
		   If curr_sum is lesser than 0, Move b towards right to increase curr_sum
		   If curr_sum == 0 => Add (a,b,c) to a set
	   2.4 If b & c cross each other, Start 2. over with i=1
		     a,b,c = S[i], S[i+1], S[-1]
	3. return all the unique triplets accumulated in the set as a list of list
'''

class Solution:
	# @param A : list of integers
	# @return a list of list of integers
	def threeSum_zero(self, S):
		if not S or len(S) < 3:
			return []

		S.sort()
		i = 0
		triplets = set()
		while i < len(S)-2:
			l = i+1
			r = len(S)-1
			while l < r:
				curr_sum = S[i] + S[l] + S[r]

				if curr_sum < 0:
					# Increase curr_sum
					l += 1
				elif curr_sum > 0:
					# Decrease curr_sum
					r -= 1
				else: # Found a match
					triplets.add((S[i], S[l], S[r]))
					l += 1
					r -= 1
			i += 1

		# convert set to a list of list
		return sorted(map(lambda x:list(x), triplets))


if __name__ == '__main__':
	s = Solution()
	assert s.threeSum_zero([-1,0,1,2,-1,-4]) == [[-1,-1,2], [-1,0,1]]
	assert s.threeSum_zero([1,2,3,4,-5]) == [[-5,1,4], [-5,2,3]]
	assert s.threeSum_zero([1,2,3,4,5]) == []

