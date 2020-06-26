'''
https://www.interviewbit.com/problems/minimize-the-absolute-difference/

Minimize the absolute difference

Given three sorted arrays A, B and Cof not necessarily same sizes.

Calculate the minimum absolute difference between the maximum and minimum number from the triplet a, b, c such that a, b, c belongs arrays A, B, C respectively.
i.e. minimize | max(a,b,c) - min(a,b,c) |.

Example :
Input:
A : [ 1, 4, 5, 8, 10 ]
B : [ 6, 9, 15 ]
C : [ 2, 3, 6, 6 ]

Output:
1

Explanation:
We get the minimum difference for a=5, b=6, c=6 as | max(a,b,c) - min(a,b,c) | = |6-5| = 1.
'''

'''
Solution Outline:
    1. Start with a,b,c = max(A,B,C) ==> i, j, k = nA-1, nB-1, nC-1
       1.1  minAbsDiff == INFINITY
    2. current minAbsDiff = |max(a,b,c) - min(a,b,c)|
       if current min < minAbsDiff so far, update minAbsDiff
       To minimize minAbsDiff further, pick the max(A[i], B[j], C[k]) and move i/j/k to the left
        to see if we can get a better minAbsDiff


Sample run:
          0  1  2  3  4
    A : [ 1, 4, 5, 8, 10 ]
    B : [ 6, 9, 15 ]
    C : [ 2, 3, 6, 6 ]

    a,b,c = 10, 15, 6
    i, j, k = 4, 2, 3
    minAbsDiff = INFINITY
    
    a,b,c = 10,15,6
     curr = max() - min() = 15-6 = 9 < minAbsDiff
     minAbsDiff = 9
     max() is from B, try reducing B
     j = 1

    a,b,c = 10, 9, 6
     curr = max() - min() = 10-6 = 4 < minAbsDiff
     minAbsDiff = 4
     max() is from A, try reducing A
     i = 3

    a,b,c = 8, 9, 6
     curr = max() - min() = 9-6 = 3 < minAbsDiff
     minAbsDiff = 3
     max() is from B, try reducing B
     j = 0

    a,b,c = 8, 6, 6
     curr = max() - min() = 8-6 = 2 < minAbsDiff
     minAbsDiff = 2
     max() is from A, try reducing A
     i = 2

    a,b,c = 5, 6, 6
     curr = max() - min() = 6-5 = 1 < minAbsDiff
     minAbsDiff = 1
     max() is from B, try reducing B
     j = -1
 return (5,6,6), minAbsDiff = 1
'''
class Solution:
	def find_minAbsDiff(self, A, B, C):
		nA, nB, nC = len(A), len(B), len(C)
		i, j, k = nA-1, nB-1, nC-1

		minAbsDiff = abs(max(A[i], B[j], C[k]) - min(A[i], B[j], C[k]))
		while i>=0 and j>=0 and k>=0:
			currAbsDiff = abs(max(A[i], B[j], C[k]) - min(A[i], B[j], C[k]))
			if currAbsDiff < minAbsDiff:
				minAbsDiff = currAbsDiff

			if A[i] == max(A[i], B[j], C[k]):
				i -= 1
			elif B[j] == max(A[i], B[j], C[k]):
				j -= 1
			else: #  C[k] == max(A[i], B[j], C[k]):
				k -= 1

		return minAbsDiff


if __name__ == '__main__':
	s = Solution()
	assert s.find_minAbsDiff(
			[1,4,5,8,10],
			[6,9,15],
			[2,3,6,6]) == 1 # [5,6,6]
	assert s.find_minAbsDiff(
			[5,8,10,15],
			[6,15,17,78,80],
			[2,3,7,8,8,9,10]) == 2 # [5,6,3]

