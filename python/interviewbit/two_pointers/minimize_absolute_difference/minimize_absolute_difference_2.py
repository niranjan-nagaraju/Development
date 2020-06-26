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
    1. Start with a,b,c = min(A,B,C) ==> i, j, k = 0,0,0
       1.1  minAbsDiff == INFINITY
    2. current minAbsDiff = |max(a,b,c) - min(a,b,c)|
       if current min < minAbsDiff so far, update minAbsDiff
       To minimize minAbsDiff further, pick the min(A[i], B[j], C[k]) and move i/j/k to the right
        to see if we can get a better minAbsDiff


Sample run:
          0  1  2  3  4
    A : [ 1, 4, 5, 8, 10 ]
    B : [ 6, 9, 15 ]
    C : [ 2, 3, 6, 6 ]

    a,b,c = 1,6,9
    i, j, k = 0,0,0
    minAbsDiff = INFINITY
    
    a,b,c = 1,6,2
     curr = max() - min() = 6-1 = 5 < minAbsDiff
     minAbsDiff = 5
     min() is from A, try increasing A
     i = 1

    a,b,c = 4, 6, 2
     curr = max() - min() = 6-2 = 4 < minAbsDiff
     minAbsDiff = 4
     min() is from A, try increasing A
     i = 2

    a,b,c = 5, 6, 2
     curr = max() - min() = 6-2 = 4 == minAbsDiff
     minAbsDiff = 4
     min() is from C, try increasing C
     k = 1

    a,b,c = 5, 6, 3
     curr = max() - min() = 6-3 = 3 < minAbsDiff
     minAbsDiff = 3
     min() is from C, try increasing C
     k = 2

    a,b,c = 5, 6, 6
     curr = max() - min() = 6-5 = 1 < minAbsDiff
     minAbsDiff = 1
     min() is from A, try increasing A
     i = 3

    a,b,c = 8, 6, 6
     curr = max() - min() = 8-6 = 2 > minAbsDiff
     min() is from B, try increasing B
     j = 1

    a,b,c = 8, 9, 6
     curr = max() - min() = 9-6 = 3 > minAbsDiff
     min() is from C, try increasing C
     k = 3

    a,b,c = 8, 9, 6
     curr = max() - min() = 9-6 = 3 > minAbsDiff
     min() is from C, try increasing C
     k = 4

	 k runs out of array C size
 return (5,6,6), minAbsDiff = 1
'''
class Solution:
	def find_minAbsDiff(self, A, B, C):
		i = j = k = 0

		minAbsDiff = abs(max(A[i], B[j], C[k]) - min(A[i], B[j], C[k]))
		while True:
			try:
				currAbsDiff = abs(max(A[i], B[j], C[k]) - min(A[i], B[j], C[k]))
				if currAbsDiff < minAbsDiff:
					minAbsDiff = currAbsDiff

				if A[i] == min(A[i], B[j], C[k]):
					i += 1
				elif B[j] == min(A[i], B[j], C[k]):
					j += 1
				else: #  C[k] == min(A[i], B[j], C[k]):
					k += 1
			except IndexError:
				return minAbsDiff

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

