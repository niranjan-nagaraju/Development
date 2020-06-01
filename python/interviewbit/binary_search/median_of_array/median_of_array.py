'''
https://www.interviewbit.com/problems/median-of-array/

Median of Array

There are two sorted arrays A and B of size m and n respectively.
Find the median of the two sorted arrays ( The median of the array formed by merging both the arrays ).
The overall run time complexity should be O(log (m+n)).

Sample Input
A : [1 4 5]
B : [2 3]

Sample Output
3

NOTE: IF the number of elements in the merged array is even, then the median is the average of (n/2)th and (n/2 + 1)th element.
For example, if the array is [1 2 3 4], the median is (2 + 3) / 2.0 = 2.5 
'''

'''
Solution Outline:
	Let len(A) = m, len(B) = n 
	Partition A and B into 4 parts, (Al, Ar, Bl, Br)
	  Al = A[0..x], 
	  Ar = A[x+1,.., m-1]
	  Bl = B[0..y],
	  Br = B[y+1, .., n-1]

    x and y are chosen s.t, 
	  len(Al) + len(Br) == len(Ar) + len(Bl) if (m+n) is even,
	  len(Al)+len(Br) == len(Ar) + len(Bl) + 1 if (m+n) is odd
	 
	 Move the partition points, (x,y) left or right while still retaining the above property
	 until we find an (x,y)

	 A0, A1, A2, ... Ax    |  Ax+1, Ax+2, .... Am-1
	 B0, B1, B2, ... By    |  By+1, By+2, .... Bn-1

	 s.t, 
	 Ax <= By+1 and
	 By <= Ax+1

	Then median would be (maxL+minR)/2, where maxL = max(Ax, By), minR = min(Ax+1, By+1) if (m+n) is even
	if (m+n) is odd, (left side would have one element more than the right side of the partition)
	  => median = max(Ax, By)  [median of a list of n-numbers would be the n/2th element if n is odd]

	Otherwise,
	  if Ax > By+1
	    Move towards left in A
	  else
	    Move towards right in A


Sample run 1:
	i:  0  1   2   3   4   5 
	A: [1, 3,  8,  9,  15]
	B: [7, 11, 18, 19, 21, 25]
	m = 5, n = 6

    Find partition points (x,y) s.t., (x+y) == (m+n+1)/2 == (5+6+1)/2 == 6

	Initially,
	x = (0+4)/2 == 2
	x+y == 6 => y == 4
	partition:
	1  3         |   8  9  15
	7 11 18 19   |  21 25
	3 <= 21? YES
	19 <= 8? NO
	 Ax = 3 < By+1 = 21
	  Move towards right in A
	  x = (3+4)/2 == 3

	x+y == 6 => y = 3
	partition:
	1  3  8      |   9 15
	7 11 18      |  19 21 25
	8 <= 19? YES
	18 <= 9? NO
	Ax = 8 < By+1 = 19
	  move towards right in A
	  x = (4+4)/2 == 4

	y = 6-x = 6-4 = 2
	partition:
	1 3  8  9  |   15
	7 11       |   18 19 21 25
	9 <= 18? YES
	11 <= 15? YES
	Ax = 9, By = 11
	(n+m) is odd
	=> median = max(Ax,By) = max(9, 11) = 11

    verify:
	             0  1  2  3  4   5   6   7   8   8  10
	  combined: [1, 3, 7, 8, 9, 11, 15, 18, 19, 21, 25]
	  median: combined[10/2] == combined[5] == 11
	


Sample run 2:
	i:   0   1   2   3   4   5
	A: [23, 26, 31, 35]
	B: [ 3,  5,  7,  9, 11, 16]

	m = 4
	n = 6
	(n+m) is even

	Initially,
	x = (0+3)/2 = 1
	(m+n+1)/2 == (11)/2 = 5
	y = 5-x  = 5-1 = 4
	partition:
	23        |  26 31 35
	 3 5 7 9  |  11 16
	23 <= 11? NO
	Ax = 23 > By+1 = 11
	  move towards left in A
	  x = (0+1)/2 = 0
	  
	y = 5-x = 5
	partition:
	            |  23 26 31 35
	 3 5 7 9 11 |  16
	Ax = {}, By+1 = 16 => Ax <= By+1
	By = 11 <= Ax+1 = 23
	maxL = 11, minR = 16
	median = (maxL+minR)/2 = (11+16)/2 = 13.5

	Verify:
			   0  1  2  3   4  	5   6   7   8   9
	combined: [3, 5, 7, 9, 11, 16, 23, 26, 31, 35]
	median = (combined[9/2]+combined[9/2+1])/2 == (combined[4]+combined[5])/2 = (11+16)/2 = 13.5


Sample run 3:
	A: [1, 4, 5]
	B: [2, 3]

	A: [2,3]
	B: [1,4,5]
	m = 2
	n = 3
	(n+m+1)/2 = 6/2 = 3

	Initially,
	startx = 0, endx = 1
	x = (startx+endx)/2 = 1
	y = 3-x = 2
	partition:
	2    |  3
	1 4  |  5
	2 <= 5? YES
	4 <= 3> NO
	2 < 5 => Move to the right of x
	startx = x+1 = 1

	x = (startx+endx)/2 = (1+1)/2 == 1
	y = 3-x = 2
	partition:
	2    |  3
	1 4  |  5
	2 <= 5? YES
	4 <= 3> NO
	2 < 5 => Move to the right of x
	startx = x+1 = 2

	startx > endx ?????!

Instead initialize endx = m = 2
	Initially,
	startx = 0, endx = 2
	x = (startx+endx)/2 = 1
	y = 3-x = 2
	partition:
	2    |  3
	1 4  |  5
	2 <= 5? YES
	4 <= 3> NO
	2 < 5 => Move to the right of x
	startx = x+1 = 1

	x = (startx+endx)/2 = (1+2)/2 == 1
	y = 3-x = 2
	partition:
	2    |  3
	1 4  |  5
	2 <= 5? YES
	4 <= 3> NO
	2 < 5 => Move to the right of x
	startx = x+1 = 2

	x = (startx+endx)/2 = (2+2)/2 == 2
	y = 3-x = 1
	partition:
	2 3  |  
	1    |  4 5
	3 <= 4? YES
	1 <= INF ? YES

	(n+m) is odd
	=> median = max(Ax, By)
	Ax = 3, By = 1
	median = max(3,1) == 3
'''
import sys
class Solution:
	def find_median_of_two_sorted_arrays(self, A, B):
		# A is expected to be the smaller array
		if len(A) > len(B):
			A, B = B, A

		m = len(A)
		n = len(B)

		startx = 0

		# Initialize endx to m and not (m-1)
		# because 'x' goes from 0 to m
		# 0 => 0 elements of A to the left
		# m => all elements of A to the right
		endx = m

		while startx <= endx:
			x = (startx+endx)/2
			y = (m+n+1)/2 - x

			# if the left side of the partition for either A or B has run out of elements, Initialize Ax/By to -INFINITY
			# if we ran out for the right side, Initialize Ax_1/By_1 to +INFINITY
			Ax = A[x-1] if (x-1) >= 0 else None
			Ax_1 = A[x] if (x < m) else sys.maxint

			By = B[y-1] if (y-1) >= 0 else None
			By_1 = B[y] if (y < n) else sys.maxint


			if Ax <= By_1 and By <= Ax_1:
				if (n+m) & 1:
					# n+m is odd
					return max(Ax, By)
				else:
					# n+m is even
					return (max(Ax,By) + min(Ax_1, By_1))/2.0
			elif Ax > By_1:
				# Move to the left of A
				endx = x-1
			else:
				# Move to the right of A
				startx = x+1

		# We wouldn't reach here
		# except maybe when the arrays aren't really sorted
		return None



if __name__ == '__main__':
	s = Solution()
	assert s.find_median_of_two_sorted_arrays([1,3,8,9,15], [7,11,18,19,21,25]) == 11
	assert s.find_median_of_two_sorted_arrays([3,5,7,9,11,16], [23,26,31,35]) == 13.5
	assert s.find_median_of_two_sorted_arrays([1,4,5], [2,3]) == 3

