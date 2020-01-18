#encoding: utf-8

'''
https://leetcode.com/problems/next-greater-element-iii/

556. Next Greater Element III

Given a positive 32-bit integer n, you need to find the smallest 32-bit integer which has exactly the same digits existing in the integer n and is greater in value than n. If no such positive 32-bit integer exists, you need to return -1.

Example 1:
Input: 12
Output: 21
 

Example 2:
Input: 21
Output: -1
'''


'''
Solution Outline:
	Let X = {x₁, x₂, x₃, . . ., xᵢ, xᵢ₊₁ ..., xn}
	let y = {xᵢ₊₁ ..., xn} be a non-increasing sequence
	      xᵢ₊₁ ≥ xᵢ₊₂ ≥ . . . ≥ xn
	therefore xᵢ < xᵢ+1
	  To get the next greater number after X using an arrangement of x₁ .. xn,
	     i. Find xⱼ s.t its the smallest in 'y' that is greater than xᵢ
		    [since y is a decreasing sequence, this can be achieved by scanning R->L until we find an xⱼ > xᵢ]
			for e.g.
			X = 1 3 5 4 2
			y = 5 4 2, xⱼ = 4 > 3
		ii. If xⱼ and xᵢ are swapped, y': {xᵢ₊₁, ..., xᵢ , ... xn} is still a non-increasing sequence with xᵢ perfectly replacing xⱼ
		    y': {xᵢ₊₁, ..., xᵢ , ... xn}
		    X = 1 3 5 4 2
			y = 5 4 2
			xᵢ = 3,  xⱼ = 4
			y': 5 3 2 is still non-increasing
		iii. xⱼ replacing xᵢ will ensure the new number will be greater but with an optimal replacement for xᵢ
		iv. y' is still a non-increasing sequence, if y' is reversed, we'll get a non-decreasing sequence
		    yᵣ: {xn, ..., xᵢ ,  ... xᵢ₊₁}
			X', the next greater number can then be
			X': {x₁, x₂, x₃, . . ., xⱼ} + yᵣ -- ensures X' is the minimum  arrangement of x1.. xn but > X
			  : {x₁, x₂, x₃, . . ., xⱼ + xn, ..., xᵢ ,  ... xᵢ₊₁}
			X : 1 3 5 4 2
			y = 5 4 2
			xᵢ = 3,  xⱼ = 4
			swap xᵢ = 3 and  xⱼ = 4 in y'
			y': 5 3 2
			yᵣ: 2 3 5
			X' = {x₁, x₂, x₃, . . ., xⱼ} + yᵣ
			   = 1 4 2 3 5
'''


class Solution(object):
	def nextGreaterElement(self, n):
		"""
		:type n: int
		:rtype: int
		"""
		nStr = str(n)
		i = len(nStr) - 1
		while i > 0 and nStr[i] <= nStr[i-1]:
			i = i - 1

		if i == 0:
			return -1

		i = i - 1

		# find xⱼ > xᵢ from x[i:n]
		j = len(nStr)-1
		while nStr[j] <= nStr[i]:
			j -= 1

		# replace xⱼ with xᵢ in y = {xᵢ₊₁ ..., xn}
		# Next greater number X_ = {x₁, x₂, x₃, . . ., xⱼ} + yᵣ
		nge_str = nStr[:i] + nStr[j] + (nStr[i+1:j] + nStr[i] + nStr[j+1:])[::-1]

		nge = int(nge_str)

		# NGE needs to be a 32-bit integer
		# return -1 if nge is bigger than 32-bit signed integer (0x7f ff ff ff)
		return nge if nge < (1<<31) else -1



if __name__ == '__main__':
	s = Solution()
	assert s.nextGreaterElement(13542) == 14235

	nges = [1243, 1324, 1342, 1423, 1432, 2134, 2143, 2314, 2341, 2413, 2431, 3124, 3142, 3214, 3241, 3412, 3421, 4123, 4132, 4213, 4231, 4312, 4321, -1]
	n = 1234
	i = 0
	while n != -1:
		n = s.nextGreaterElement(n)
		assert n == nges[i]
		i += 1

	assert s.nextGreaterElement(2147483647) == -1 # overflows 32-bit
	assert s.nextGreaterElement(1999999999) == -1 # overflows 32-bit

