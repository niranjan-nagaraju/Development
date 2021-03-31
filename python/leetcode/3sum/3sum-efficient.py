#encoding: utf-8

'''
https://leetcode.com/problems/3sum/

Return all unique triplets that add upto 0
a + b + c = 0

Given array nums = [-1, 0, 1, 2, -1, -4],

A solution set is:
[
  [-1, 0, 1],
  [-1, -1, 2]
]
'''


'''
Solution Outline: O(n**2)
    1. Sort the input list, A
    2. Let a,b,c be a triplet in A s.t a+b+c == 0
        and i, j, k (i < j < k) be their respective indices in A
    3. Let f(A, lb, ub sum) be a method that returns indices (i, j) in A[lb..ub] s.t A[i]+A[j] == sum
        -> this can be solved in O(n) time on a sorted array, A
    4. For each a in A | A[i] = a, check if an index-pair (j,k) exists in A[i+1 .. ]
        such that A[j]+A[k] == -a using f()
            if found, add them to the list of result-triplets.
'''

class Solution(object):
	def pairs_with_target_sum(self, nums, lb, ub, target):
		pairs = []
		while lb < ub:
			curr_sum = nums[lb] + nums[ub]
			if curr_sum == target:
				pairs.append((lb, ub))
			if curr_sum < target:
				lb += 1
			else: # curr_sum > target
				ub -= 1
		return pairs

	def threeSum(self, nums):
		nums = sorted(nums)
		n = len(nums)
		results = set()
		for i in xrange(n):
			pairs = self.pairs_with_target_sum(nums, i+1, n-1, -nums[i])
			for (j,k) in pairs:
				results.add((nums[i], nums[j], nums[k]))
		return [[a,b,c] for a,b,c in results]

if __name__ == '__main__':
	sol = Solution()
	assert sol.threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]]
	assert sol.threeSum([1,2,3,-1,0]) == [[-1, 0, 1]]
	assert sol.threeSum([0,0,0]) == [[0,0,0]]
	assert sorted(sol.threeSum([-2,0,1,1,2])) == sorted([[-2, 0, 2], [-2, 1, 1]])

