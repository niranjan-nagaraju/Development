'''
https://www.interviewbit.com/problems/single-number/

Given an array of integers, every element appears twice except for one. Find that single one.

Note: Your algorithm should have a linear runtime complexity. Could you implement it without using extra memory?

Input Format:
    First and only argument of input contains an integer array A

Output Format:
    return a single integer denoting single element

Constraints:
2 <= N <= 2 000 000  
0 <= A[i] <= INT_MAX

Example Input 1:
    A = [1, 2, 2, 3, 1]
Example Output 1:
    3
Explanation:
    3 occurs only once

Example Input 2:
    A = [1, 2, 2]
Example Output 2:
    1
'''

'''
Solution Outline:
	A ^ A = 0
	A ^ 0 = A
	Calculate xor of all the elements in the array,
	the duplicate elements would cancel each other out resulting in 0, singling out the unique element.
'''
class Solution:
    # @param A : tuple of integers
    # @return an integer
	def singleNumber(self, A):
		xor_sum = 0
		for x in A:
			xor_sum = xor_sum ^ x

		return xor_sum


	# Use reduce to calculcate xor of the entire array
	def singleNumber_2(self, A):
		return reduce(lambda acc,x: acc^x, A, 0)


if __name__ == '__main__':
	s = Solution()
	assert s.singleNumber([1, 2, 2, 3, 1]) == 3
	assert s.singleNumber([1, 2, 2]) == 1
	assert s.singleNumber([1,2,3,4,3,1,2]) == 4

	assert s.singleNumber_2([1, 2, 2, 3, 1]) == 3
	assert s.singleNumber_2([1, 2, 2]) == 1
	assert s.singleNumber_2([1,2,3,4,3,1,2]) == 4



