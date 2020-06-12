'''
https://www.interviewbit.com/problems/merge-two-sorted-lists-ii/

Merge Two Sorted Lists II

Given two sorted integer arrays A and B, merge B into A as one sorted array.

Note: You have to modify the array A to contain the merge of A and B. Do not output anything in your code.
TIP: C users, please malloc the result into a new array and return the result. 
If the number of elements initialized in A and B are m and n respectively, the resulting size of array A after your code is executed should be m + n

Example :

Input : 
         A : [1 5 8]
         B : [6 9]

Modified A : [1 5 6 8 9]
'''

'''
Solution Outline:
	1. Use the merge algorithm to merge the two arrays into an auxiliary array C
	2. Since A needs to contain the merged result, copy C into A
	NOTE: Modifying A in-place to insert/place B's elements into A might result in quadratic time-efficiency.
'''

class Solution:
	# Merge B into A keeping A sorted
	def merge_two_sorted_lists(self, A, B):
		def merge(A, B):
			C = [0]*(len(A)+len(B))
			k = 0
			iA = iB = 0
			while iA < len(A) and iB < len(B):
				if A[iA] <= B[iB]:
					C[k] = A[iA]
					iA += 1
				else:
					C[k] = B[iB]
					iB += 1
				k += 1

			# We still might have elements remaining in A
			# if there are, all of them are > B
			while iA < len(A):
				C[k] = A[iA]
				iA += 1
				k += 1

			# We still might have elements remaining in B
			# if there are, all of them are > A
			while iB < len(B):
				C[k] = B[iB]
				iB += 1
				k += 1

			return C

		C = merge(A, B)
		
		# Copy C into A until A's current length is filled
		# then start appending to the end
		for i in xrange(len(A)):
			A[i] = C[i]

		i += 1
		while i < len(C):
			A.append(C[i])
			i += 1

		return A



if __name__ == '__main__':
	s = Solution()
	A = [1,5,8]
	B = [6,9]
	s.merge_two_sorted_lists(A, B)
	assert A == [1,5,6,8,9]



