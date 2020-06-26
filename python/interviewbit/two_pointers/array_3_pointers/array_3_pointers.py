#encoding: utf-8
'''
https://www.interviewbit.com/problems/array-3-pointers/

Array 3 Pointers

You are given 3 arrays A, B and C. All 3 of the arrays are sorted.

Find i, j, k such that :
max(abs(A[i] - B[j]), abs(B[j] - C[k]), abs(C[k] - A[i])) is minimized.
Return the minimum max(abs(A[i] - B[j]), abs(B[j] - C[k]), abs(C[k] - A[i]))

**abs(x) is absolute value of x and is implemented in the following manner : **
      if (x < 0) return -x;
      else return x;

Example :
Input : 
        A : [1, 4, 10]
        B : [2, 15, 20]
        C : [10, 12]
Output : 5 
         With 10 from A, 15 from B and 10 from C.
'''

'''
Solution Outline:
    a,b,c | a ∈ A, b ∈ B, c ∈ C
    We need to minimize a,b,c | max(|a-b|, |b-c|, |c-a|)
    max(|a-b|, |b-c|, |c-a|) == max(a,b,c)-min(a,b,c)
    => we need to minimize max(a,b,c)-min(a,b,c)

    This is similar to
    https://www.interviewbit.com/problems/minimize-the-absolute-difference/

    1. Start with a,b,c = min(A,B,C) ==> i, j, k = 0,0,0
       1.1  minAbsDiff == INFINITY
    2. current minAbsDiff = |max(a,b,c) - min(a,b,c)|
       if current min < minAbsDiff so far, update minAbsDiff
       To minimize minAbsDiff further, pick the min(A[i], B[j], C[k]) and move i/j/k to the right
        to see if we can get a better minAbsDiff
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

