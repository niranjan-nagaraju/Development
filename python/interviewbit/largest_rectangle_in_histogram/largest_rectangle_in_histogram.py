#encoding: utf-8
'''
https://www.interviewbit.com/problems/largest-rectangle-in-histogram/

Largest Rectangle in Histogram

Given an array of integers A of size N. A represents a histogram i.e A[i] denotes height of
the ith histogram’s bar. Width of each bar is 1.
       _
     _| |
    |/|/|
    |/|/|  _
 _  |/|/|_| |
| |_|/|/| | |
|_|_|/|/|_|_|

Above is a histogram where width of each bar is 1, given height = [2,1,5,6,2,3].
The largest rectangle is shown in the shaded area, which has area = 10 unit.

Find the area of largest rectangle in the histogram.

Input Format
The only argument given is the integer array A.

Output Format
Return the area of largest rectangle in the histogram.

For Example
Input 1:
    A = [2, 1, 5, 6, 2, 3]
Output 1:
    10
Explanation 1:
	The largest rectangle is shown in the shaded area, which has area = 10 unit.
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

	  x, idx = pop()
	  area = (i - idx) * x if stack still has elements in them
	  otherwise
	  area = i * x
	     WHY? because a single element in the stack means its the smallest number so far, 
		 so the area involving it has to be i * x
		   for eg,
		      stack: [(1,3)], i: 5
			  we have processed 5 bars, and the stack has (1,3) [bar of height 1 at index 3)
			  area = 1 * 6
	e.g,
	  bars: [1,2,3,1]
	  if the stack is [(1,0), (2,1), (3,2)]. i = 3
	    pop (3,2), area = (i-2)*3 = 1*3 = 3
		pop(2,1), area = (i-1)*2 = 2*2 == 4
	 
	 At the end, process all the increasing bars in the stack until the stack is empty.
	 [(1,4), (3,5)] for eg,
	   reduce as earlier.
		


Sample run 1:
============
	histogram: [2,1,5,6,2,3]
	max area = 0
	
	stack: []
	i: 0 (2)
	  stacktop = empty
	  push (2,0)
	  stack: [(2,0)]

	i: 1 (1)
	  stacktop = 2 > 1
	  x, idx = pop() -> 2, 0
	  stack: [] is empty
	  area = (i) * x == 1 * 2 == 2
	  max area = 2
	  push (1,1)
	  stack: [(1,1)]

	i: 2 (5)
	  stacktop = 1 < 5
	  push (5,2)
	  stack: [(5,2), (1,1)]

	i: 3 (6)
	  stacktop = 5 < 6
	  push (6,3)
	  stack: [(6,3), (5,2), (1,1)]

	i: 4 (2)
	  stacktop = 6 > 2
	  x, idx = pop() -> 6, 3
	  stack: [(5,2), (1,1)]
	  area = (i - idx) * x == (4-3) * 6 == 6
	  max area = 6

	  stacktop = 5 > 2
	  x, idx = pop() -> 5, 2
	  stack: [(1,1)]
	  area = (i - idx) * x == (4-2) * 5 == 10
	  max area = 10

	  stacktop = 1  < 2
	  push(2,4)
	  stack: [(2,4), (1,1)]

	i: 5 (3)
	  stacktop = 2 < 3
	  push (3,5)
	  stack: [(3,5), (2,4), (1,1)]
   
    i: 6 -> EOF
    
	Reduce until stack is empty
	  x, idx = pop() -> 3, 5
	  stack: [(2,4), (1,1)]
	  area = (i - idx) * x == (6-5) * 3 == 3 < max area

	  x, idx = pop() -> 2, 4
	  stack: [(1,1)]
	  area = (i - idx) * x == (6-4) * 2 == 4 < max area

	  x, idx = pop() -> 1, 1
	  stack: []
	  area = (i) * x == (6) * 1 == 6 < max area
	max area = 10



Sample run 2:
============
	histogram: [3,2,1,2,3]

	max area = 0
	stack = []

	i: 0 (3)
	  stacktop = empty
	  push (3,0)
	  stack: [(3,0)]

	i: 1 (2)
	  stacktop = 3 > 2
	  x, idx = pop() -> 3, 2
	  stack: [] -> empty
	  area = i * x = 1 * 3 = 3
	  max area = 3
	  push (2,1)
	  stack: [(2,1)]

	i: 2 (1)
	  stacktop = 2 > 1
	  x, idx = pop() -> 2, 1
	  stack: [] -> empty
	  area = i * x = 2 * 2 = 4
	  max area = 4
	  push (1, 2)
	  stack: [(1,2)]

	i: 3 (2)
	  stacktop = 1 < 2
	  push (2,3)
	  stack: [(2,3), (1,2)]

	i: 4 (3)
	  stacktop = 2 < 3
	  push (3,4)
	  stack: [(3,4), (2,3), (1,2)]

	i: 5 -> EOF
	Reduce until stack is empty
	  x, idx = pop() -> 3, 4
	  stack: [(2,3), (1,2)]
	  area = (i - idx) * x = (5-4)*3 = 3 < max area

	  x, idx = pop() -> 2, 3
	  stack: [(1,2)]
	  area = (i - idx) * x = (5-3)*2 = 4 == max area

	  x, idx = pop() -> 1, 2
	  stack: [] -> empty
	  area = (i) * x = (5)*1 = 5 > max area
	  max area = 5
	max area = 5
'''

class Solution:
	def largest_rectangle(self, histogram):
		stack = []
		max_area = 0

		i = 0
		while i < len(histogram):
			while stack and stack[-1][0] >= histogram[i]:
				x, idx = stack.pop()
				if not stack:
					idx = 0

				area = (i-idx) * x
				if area > max_area:
					max_area = area

			stack.append((histogram[i], i))

			i += 1

		while stack:
			x, idx = stack.pop()
			if not stack:
				idx = 0

			area = (i-idx) * x
			if area > max_area:
				max_area = area

		return max_area



if __name__ == '__main__':
	s = Solution()
	assert s.largest_rectangle([2,1,5,6,2,3]) == 10
	assert s.largest_rectangle([3,2,1,2,3]) == 5
	print s.largest_rectangle([6, 2, 5, 4, 5, 1, 6]) # prints 10 correct answer should be 12 -- FAIL


