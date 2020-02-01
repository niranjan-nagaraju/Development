'''
https://leetcode.com/problems/largest-rectangle-in-histogram/

84. Largest Rectangle in Histogram

Given n non-negative integers representing the histogram's bar height where the width of each bar is 1,
find the area of largest rectangle in the histogram.
       _
     _| |
    |/|/|
    |/|/|  _
 _  |/|/|_| |
| |_|/|/| | |
|_|_|/|/|_|_|

Above is a histogram where width of each bar is 1, given height = [2,1,5,6,2,3].

The largest rectangle is shown in the shaded area, which has area = 10 unit.

Example:
Input: [2,1,5,6,2,3]
Output: 10
'''

'''
Solution Outline:
	Use a stack to store non-decreasing bar heights.
	Push bars as long as they are > stacktop.
	Each entry in the stack represents the minimum from the bar to the right of it.
	  for e.g.,
	    [(1,0), (2,1), (3,2)]
		[3] is the minimum bar from 3 to 4
		[2] is the minimum bar from 2 to 4
		[1] is the minimum bar overall.
	If the current bar < stacktop, its time to reduce.
	pop until the stack is non-decreasing when current bar is pushed.
	  for each popped item, check the rectangular area possible from it.

	if the last two entries in the stack were say,
	  . . ., (x, i), (y, j)   [therefore, y >= x]
	     If we now encounter a (z, k) s.t. z < y, then we know every bar between (i, j) is atleast y
		 Therefore, to reduce we remove (y, j) from the stack, and the area involving y would be
		   (k - i - 1) * y
		   for e,g,
			 histogram: [,..,1,4,5,4, ..]
			 stack: [(1,1), (4,2), (5,3)], k = 4
			   y, j = 5, 3
			   x, i = 4, 2
			   area = (k - i - 1) * y == (4-2-1)*5 == 5
			   
		 However, if there was no such (x, i) then y is the smallest element until z
		 and the area would be
		   k * y
		    
	 At the end, process all the increasing bars in the stack until the stack is empty.
	 [(1,4), (3,5)] for eg,
	   reduce as earlier.
		


Sample run 1:
============
	histogram: [2,1,5,6,2,3]
	max area = 0
	
	stack: []
	k: 0 (2)
	  stacktop = empty
	  push (2,0)
	  stack: [(2,0)]

	k: 1 (1)
	  stacktop = 2 > 1
	  y, j = pop() -> 2, 0
	  stack: [] is empty
	  area = (k) * y == 1 * 2 == 2
	  max area = 2
	  push (1,1)
	  stack: [(1,1)]

	k: 2 (5)
	  stacktop = 1 < 5
	  push (5,2)
	  stack: [(5,2), (1,1)]

	k: 3 (6)
	  stacktop = 5 < 6
	  push (6,3)
	  stack: [(6,3), (5,2), (1,1)]

	k: 4 (2)
	  stacktop = 6 > 2
	  y, j = pop() -> 6, 3
	  stack: [(5,2), (1,1)]
	  x, i = 5, 2
	  area = (k - i - 1) * y == (4-2-1) * 6 == 6
	  max area = 6

	  stacktop = 5 > 2
	  y, j = pop() -> 5, 2
	  stack: [(1,1)]
	  x, i = 1,1
	  area = (k - i - 1) * y == (4-1-1) * 5 == 10
	  max area = 10

	  stacktop = 1  < 2
	  push(2,4)
	  stack: [(2,4), (1,1)]

	k: 5 (3)
	  stacktop = 2 < 3
	  push (3,5)
	  stack: [(3,5), (2,4), (1,1)]
   
    k: 6 -> EOF
    
	Reduce until stack is empty
	  y, j = pop() -> 3, 5
	  stack: [(2,4), (1,1)]
	  x, i = 2, 4
	  area = (k - i - 1) * y == (6-4-1) * 3 == 3 < max area

	  y, j = pop() -> 2, 4
	  stack: [(1,1)]
	  x, i = 1, 1
	  area = (k-i-1) *y  == (6-1-1) * 2 == 8 < max area

	  y, j = pop() -> 1, 1
	  stack: []
	  area = (k) * y == (6) * 1 == 6 < max area
	max area = 10



Sample run 2:
============
	histogram: [3,2,1,2,3]

	max area = 0
	stack = []

	k: 0 (3)
	  stacktop = empty
	  push (3,0)
	  stack: [(3,0)]

	k: 1 (2)
	  stacktop = 3 > 2
	  y, j = pop() -> 3, 2
	  stack: [] -> empty
	  area = k * y = 1 * 3 = 3
	  max area = 3
	  push (2,1)
	  stack: [(2,1)]

	k: 2 (1)
	  stacktop = 2 > 1
	  y, j = pop() -> 2, 1
	  stack: [] -> empty
	  area = k * y = 2 * 2 = 4
	  max area = 4
	  push (1, 2)
	  stack: [(1,2)]

	k: 3 (2)
	  stacktop = 1 < 2
	  push (2,3)
	  stack: [(2,3), (1,2)]

	k: 4 (3)
	  stacktop = 2 < 3
	  push (3,4)
	  stack: [(3,4), (2,3), (1,2)]

	k: 5 -> EOF
	Reduce until stack is empty
	  y, j = pop() -> 3, 4
	  stack: [(2,3), (1,2)]
	  x, i = 2, 3
	  area = (k - i - 1) * y = (5-3-1)*3 = 3 < max area

	  y, j = pop() -> 2, 3
	  stack: [(1,2)]
	  x, i = 1, 2
	  area = (k - i - 1) * y = (5-2-1)*2 = 4 == max area

	  y, j = pop() -> 1, 2
	  stack: [] -> empty
	  area = (k) * y = (5)*1 = 5 > max area
	  max area = 5
	max area = 5
'''

class Solution:
	def largestRectangleArea(self, histogram):
		stack = []
		max_area = 0
		k = 0
		for k in xrange(len(histogram)):
			# while stacktop > current bar
			# keep reducing
			while stack and histogram[stack[-1]] > histogram[k]:
				j = stack.pop()
				y = histogram[j]

				i = stack[-1] if stack else None
				area = (k - i - 1) * y if stack else k * y
				if area > max_area:
					max_area = area

			# store only the indices in the stack
			stack.append(k)

		k += 1 # xrange ends at 'n-1'
		while stack:
			# Stack contains the last increasing sequence
			# from the histograms list, keep reducing
			j = stack.pop()
			y = histogram[j]

			i = stack[-1] if stack else None
			area = (k - i - 1) * y if stack else k * y
			if area > max_area:
				max_area = area

		return max_area



if __name__ == '__main__':
	s = Solution()
	assert s.largestRectangleArea([2,1,5,6,2,3]) == 10
	assert s.largestRectangleArea([3,2,1,2,3]) == 5
	assert s.largestRectangleArea([6, 2, 5, 4, 5, 1, 6]) == 12


