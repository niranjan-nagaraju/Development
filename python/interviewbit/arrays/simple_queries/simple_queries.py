'''
https://www.interviewbit.com/problems/simple-queries/

Simple Queries

You are given an array A having N integers.

You have to perform the following steps in a given order.

	1. generate all subarrays of A.
	2. take the maximum element from each subarray of A and insert it into a new array G.
	3. replace every element of G with the product of their divisors mod 1e9 + 7.
	4. sort G in descending order
	5. perform Q queries
In each query, you are given an integer K, where you have to find the Kth element in G.

Note: Your solution will run on multiple test cases so do clear global variables after using them.

Input Format
The first argument given is an Array A, having N integers.
The second argument given is an Array B, where B[i] is the ith query.

Output Format
Return an Array X, where X[i] will have the answer for the ith query.

Constraints
1 <= N <= 1e5
1 <= A[i] <= 1e5
1 <= Q <= 1e5
1 <= k <= (N * (N + 1))/2 

For Example
Input:
    A = [1, 2, 4]
    B = [1, 2, 3, 4, 5, 6]
Output:
    X = [8, 8, 8, 2, 2, 1]
   
Explanation:
    subarrays of A	  maximum element
    ------------------------------------
    1. [1]							1
    2. [1, 2]						2
    3. [1, 2, 4]					4
    4. [2]							2
    5. [2, 4]						4
    6. [4]							4

	original
	G = [1, 2, 4, 2, 4, 4]
	
	after changing every element of G with product of their divisors
	G = [1, 2, 8, 2, 8, 8]
	
	after sorting G in descending order
	G = [8, 8, 8, 2, 2, 1]
'''

import math
class Solution:
	def simple_queries(self, A, B):
		# Calculate x**y using binary exponentiation in O(logn) time
		def power(x, y) : 
			res = 1
			while y > 0: 
				if (y & 1 == 1): 
					res = (res * x) % 1000000007 
				y = (y >> 1) % 1000000007 
				x = (x * x) % 1000000007 
				  
			return res

		# return product of its divisors % 1e9+7
		def product_of_divisors(subarray_maximum):
			if not product_cache.has_key(subarray_maximum):
				# product of divisors of a number can be written as N ^ D/2,
				# where N is number and D is number of divisors of N.

				# Count number of divisors -- GeeksforGeeks
				num_d = 0
				i = 1
				while i * i <= subarray_maximum : 
					if (subarray_maximum % i == 0) : 
						# If factors are equal, 
						# count only once 
						if (subarray_maximum / i == i) : 
							num_d = num_d + 1
						  
						# Otherwise count both 
						else : 
							num_d = num_d + 2
					i = i + 1

				# Calculate product of divisors
				prod = power(subarray_maximum, num_d/2)

				# if num_d is odd, we need to multiply prod by sqrt(subarray_maximum)
				# for eg,
				# a^5/2 = a^2 * sqrt(a)
				if (num_d & 1) == 1:
					prod = (prod * (int)(math.sqrt(subarray_maximum))) % 1000000007

				product_cache[subarray_maximum] = prod
			return product_cache[subarray_maximum]


		product_cache = {}
		n = len(A)
		G = []
		for i in xrange(n):
			subarray_maximum = A[i]
			for j in xrange(i, n):
				subarray_maximum = max(subarray_maximum, A[j])
				G.append(product_of_divisors(subarray_maximum))

		G.sort(reverse=True)

		query_answers = []
		for query in B:
			query_answers.append(G[query-1])

		return query_answers


if __name__ == '__main__':
	s = Solution()
	assert s.simple_queries([1,2,4], [1,2,3,4,5,6]) == [8,8,8,2,2,1]

