'''
https://www.interviewbit.com/problems/intersection-of-sorted-arrays/

Intersection Of Sorted Arrays

Find the intersection of two sorted arrays.
OR in other words,
Given 2 sorted arrays, find all the elements which occur in both the arrays.

Example :

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 3 5]

Output : [3 3 5]

Input : 
    A : [1 2 3 3 4 5 6]
    B : [3 5]

Output : [3 5]
'''

'''
Solution Outline:
  1. Proceed as with merging two sorted arrays
     1.1 Initialize two pointers, i and j to begin at A and B respectively
     1.2 If A[i] < B[j]:
           Move i ahead
         If A[i] > B[j]:
           Move j ahead
         If A[i] == B[j]:
           Copy A[i] to output
           Move ahead both i,j
  2. When either of A or B runs out, we are done finding the intersection of A and B, return the collected output.


Sample run:
  A: [1, 3, 3, 4, 5, 6]
  B: [2, 3, 5]

  i, j = 0
  A[i] = 1
  B[j] = 2
  A[i] < B[j] => i++
    i = 1

  A[i] = A[1] = 3
  B[j] = 2
  A[i] > B[j] => j++
    j = 1
  
  A[i] = A[1] = 3
  B[j] = B[1] = 3
  A[i] = B[j]
    Output: [3]
    i++, j++
    i = 2, j = 2

  A[i] = A[2] = 3
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 3

  A[i] = A[3] = 4
  B[j] = B[2] = 5
  A[i] < B[j] => i++
    i = 4

  A[i] = A[4] = 5
  B[j] = B[2] = 5
  A[i] = B[j]
    Output: [3, 5]
    i++, j++
    i = 5, j = 3

  A[i] = A[5] = 5
  B[j] = (j = len(B)): exit

  Output: [3,5]
'''
class Solution:
	def find_intersection(self, A, B):
		i = j = 0
		intersects = []
		while i < len(A) and j < len(B):
			if A[i] < B[j]:
				i += 1
			elif A[i] > B[j]:
				j += 1
			else: # A[i] == B[j]
				intersects.append(A[i])
				i += 1
				j += 1

		return intersects


if __name__ == '__main__':
	s = Solution()
	assert s.find_intersection([1,3,3,4,5,6], [2,3,5]) == [3,5]
	assert s.find_intersection([2,3,5], [1,3,3,4,5,6]) == [3,5] # reversing A and B should yield the same output
	assert s.find_intersection([1,2,3,3,4,5,6], [3,3,5]) == [3,3,5]
	assert s.find_intersection([1,2,3,3,4,5,6], [3,5]) == [3,5]

