'''
https://www.interviewbit.com/problems/highest-product/

Highest Product


Given an array A, of N integers A.
Return the highest product possible by multiplying 3 numbers from the array.
NOTE: Solution will fit in a 32-bit signed integer.


Input Format:
The first and the only argument is an integer array A.

Output Format:
Return the highest possible product.

Constraints:
1 <= N <= 5e5

Example:
Input 1:
A = [1, 2, 3, 4]

Output 1:
24

Explanation 1:
2 * 3 * 4 = 24

Input 2:
A = [0, -1, 3, 100, 70, 50]

Output 2:
350000

Explanation 2:
70 * 50 * 100 = 350000
'''



'''
Solution Outline:
	1. The 3 highest product is the product of the biggest 3 numbers in A (if all of A is +ve).
		If A contains negative numbers, then it could be maximum( s1*s2*c, a*b*c)
			where s1, s2 are the smallest numbers in A; m1,m2,m3 are the 3 biggest numbers in A, a<=b<=c , s1 <= s2
	2. We can either sort A to find s1,s2, a,b,c and return the max of these products.
		OR compute s1,s2, a,b,c in a single pass
'''


class Solution:
	def find_highest_triplet_product(self, A):
		if not A:
			return 0

		if len(A) < 4:
			return reduce(lambda x,acc: x*acc, A, 1)

		s1 = s2 = max(A)
		a = b = c = min(A)

		for x in A:
			if x < s1:
				# replacing s1
				# Move previous s1 to s2
				s2, s1 = s1, x 
			elif x < s2:
				s2 = x

			if x > c:
				# replacing c
				# shift the previous c to b, and b to a
				a,b,c = b,c,x
			elif x > b:
				# replacing b
				# shift the previous b to a
				a,b = b,x
			elif x > a:
				a = x

		return max(s1*s2*c, a*b*c)


if __name__ == '__main__':
	s = Solution()
	assert s.find_highest_triplet_product([1, 2]) == 2
	assert s.find_highest_triplet_product([-1, 2, 3, 1]) == 6
	assert s.find_highest_triplet_product([1, -4, -5, -4]) == 20
	assert s.find_highest_triplet_product([1, -4, 5, 2, 3]) == 30
	assert s.find_highest_triplet_product([1,1,2,2,2]) == 8


