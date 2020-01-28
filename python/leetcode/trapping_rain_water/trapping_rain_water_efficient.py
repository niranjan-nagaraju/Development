'''
https://leetcode.com/problems/trapping-rain-water/

Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.
               _
       _      | |_   _
   _  | |_   _| | |_| |_
 _|_|_|_|_|_|_|_|_|_|_|_|
               _
       _      | |_   _
   _  | |W W W| | |W| |_
 _|_|W|_|_|W|_|_|_|_|_|_|

The above elevation map is represented by array [0,1,0,2,1,0,1,3,2,1,2,1]. In this case, 6 units of rain water (W) are being trapped.

Example:
	Input: [0,1,0,2,1,0,1,3,2,1,2,1]
	Output: 6
'''

'''
Solution outline:
	0. Observations:
	   We need a minimum of 3 bars to trap any water, a left anchor, a right anchor and a groove.
	   e.g., [2,1], [2,2] cannot trap any water
	   [2,1,2] can
	1. We need to keep track of the left anchor, a dip, followed by the right anchor.
	2. Use a stack to store different bar heights while ensuring that at no point in the stack, a smaller bar height is beneath  a higher one.
	   WHY?
	   because [1,2,3] is equivalent [3] as there's nothing to hold to the left.
	   As does [3,4,2,1] => [4,2,1]
	3. Keep track of both the bar height, and its position from the left so we can calculate horizontal distances between any two bars, and the water-holding capacity.	
	   [4.0,0,4] -> holds 4 2x4 == 8 units of water
	   [4,0,4] -> holds 4 units of water.
	4. To keep the stack ordered by bar height (and that the values are always decreasing from the bottom of the stack to the top), we 'reduce' the stack anytime a higher value than stacktop is found.
	   Reduce by popping all bars (< current bar height, aka the grooves) from the top until a left anchor >= current bar is found,
	     At this point, we have a left anchor, right anchor, grooves and some blocks part of these grooves that cannot trap water.
		 Once water is trapped in these grooves, they essentially become blocks of height min(left_anchor, right_anchor)
		 So we can always calculate water trapped between a left and right anchor as min(left, right) - groove
		 e.g.,
		 [3,2,1,2,4]
		 [2,1,2] -> becomes == [2,2,2] after trapping 1 unit of water
		 now [3,2,2,2,4] ==> [3,2,4], left:3, right: 4, groove = 2 => they can trap (min(3,4)-2)*3 == (3-2)*3 == 3 units of water
	5. Reduction rules:
		Until stacktop, st < current bar, curr
		   right anchor = current bar
		   pop stacktop -- These are the grooves,
		   At this point, stacktop >= popped bar => stacktop = left anchor
		   if the two bars (left, right) are separated by distance 'd', stacktop => groove
		   Actual water trapped = (min(left,right)-groove)*d
		   Total water trapped += actual water trapped
			  e.g.
			   [3,2,1,2]
			   [2,1,2] can be reduced to 1 units of water, and effectively be considered [2,2,2] after
			   so if [3,2,1,2] is followed by a [3]
			   then, [3,2,1,2,3] => [3,2,2,2,3] == 3 units of water trapped between [3..3]
			   effectively bringing the total water trapped between [3,2,1,2,3] to 4
			   --
			        _
			    _  | |
			   | | | |
			   | |_| |
			   |_|_|_|
			   e.g. [3,1,4]
			   between the 3 and 4, groove = 1
			   actual water trapped = (min(3,4)-1)*1 == (3-1)*1 == 2 units
			   --
			   e.g.
			   [3,2,1,4]
			          _
			    _    | |
			   | |_  | |
			   | | |_| |
			   |_|_|_|_|
			   stack: [1,2,3], at 4
			   > pop 1
			   stack: [2,3]
			   groove = 1, right = 4, left = stacktop = 2
			   water trapped: (min(4,2)-1)*1 == 1*1 == 1 units
			   stack: [2,3]
			   > pop 2
			   stack: [3]
			   groove = 2, right = 4, left = 3
			   water trapped: (min(4,3)-2)*2 == (3-2)*2 == 2 units
			   > pop: 3
			   groove = 3, right = 4, left = NA
	    Once all bars < current are popped,
		    push current bar

		   e.g.,
		   bar: [2,1,3]
		   At some stage, stack: [1,2], current: 3
		   1 < 3, 
		   pop: 1
		   stack: [2]
		   left = 2, right = 3, groove = 1
		   water trapped = (min(2,3)-1) * 1 == 1 units
		   stack: [2]
		   pop: 2
		   stack: []
		   left = NA, right = 3, groove = 2
		   push (3)
		   stack: [3]


Sample run 1:
=============
	   Bars: [3,2,1,0,1,4]
				      _
			_        | |
		   | |_      | |
		   | | |_   _| |
		   |_|_|_|_|_|_|

	   Stack: []
	   Total water = 0

	   i: 0
	   bar: 3
	   stack is empty
	   push 3,0
	   stack: [(3,0)]

	   i: 1
	   bar: 2
	   stacktop < 2? NO
	   push 2,1
	   stack: [(2,1), (3,0)]

	   i: 2
	   bar: 1
	   stacktop < 1? NO
	   stack: [(1,2), (2,1), (3,0)]

	   i: 3
	   bar: 0
	   stacktop < 0? NO
	   stack: [(0,3), (1,2), (2,1), (3,0)]

	   i: 4
	   bar: 1
	   stacktop == 0 < 1
	   pop: 0,3
	   groove = 0
	   stack: [(1,2), (2,1), (3,0)]
	   top: 1,2 => left = 1
	   right = 1
	   water trapped = [min(1,1)-0]*(4-2-1)  == 1
	   Total water trapped += 1 = 1
                  _ 
		_        | | 
	   | |_      | |
	   | | |_   _| |
	   |_|_|_|W|_|_|
	   --
	   stacktop == 1 < 1? NO
	   push (1,4)
	   stack: [(1,4), (1,2), (2,1), (3,0)]

	   i: 5
	   bar: 4
	   stacktop == 1 < 4
	   pop: 1,4
	   groove = 1
	   stack: [(1,2), (2,1), (3,0)]
	   top: 1,2 => left: 1
	   water trapped: min(4,1)-1 * d == 0
	   --
	   stacktop == 1 < 4
	   pop: 1,2
	   groove = 1
	   stack: [(2,1), (3,0)]
	   top: 2,1 => left: 2
	   water trapped: min(4,2)-1 * (5-1-1) == 1*3 == 3 units
                  _ 
		_        | | 
	   | |_      | |
	   | | |W W W| |
	   |_|_|_|W|_|_|
	   --
	   stacktop = 2 < 4
	   pop: 2,1
	   groove = 2
	   stack: [(3,0)]
	   top: 3,0 => left: 3
	   water trapped: min(4,3)-2 * (5-0-1) == 1*4 == 4 units
                  _ 
   		_        | | 
	   | |W W W W| |
	   | | |W W W| |
	   |_|_|_|W|_|_|



Sample run 2:
============
	Input: [0,1,0,2,1,0,1,3,2,1,2,1]
	Index:  0 1 2 3 4 5 6 7 8 9 10 11
	stack: []
	Total water: 0

	i: 0
	bar: 0
	stacktop <0? NO
	push (0,0)
	stack: [(0,0)]

	i: 1
	bar: 1
	stacktop < 1
	pop: 0,0
	stack: []
	top: NA
	push (1,1)
	Stack: [(1,1)]

	i: 2
	bar: 0
	stacktop < 0? NO
	push (0,2)
	stack: [(0,2), (1,1)]

	i: 3
	bar: 2
	stacktop = 0 < 2
	pop: 0,2
	stack: [(1,1)]
	top: 1,1 => left = 1
	groove = 0
	water trapped = [min(1,2)-0] * (3-1-1) == 1*1 = 1
	Total water trapped += 1 = 1
               _
       _      | |_   _
   _  | |_   _| | |_| |_
 _|_|W|_|_|_|_|_|_|_|_|_|
 0 1 2 3 4 5 6 7 8 9 A B
	--
	stacktop = 1 < 2
	pop: 1,1
	top: empty
	stack: []
	push (2,3)
	stack: [(2,3)]

	i: 4
	bar: 1
	stacktop = 2< 1? NO
	push (1,4)
	stack: [(1,4), (2,3)]

	i: 5
	bar: 0
	stacktop = 1< 0? NO
	push (0,5)
	stack: [(0,5), (1,4), (2,3)]

	i: 6
	bar: 1
	stacktop = 0 < 1
	pop: 0,5
	stack: [(1,4), (2,3)]
	top: 1,4 => left: 1
	groove = 0
	right = 1
	water trapped = [min(1,1)-0] * (6-4-1)  == 1*1 - 0 = 1
	Total water trapped += 1 = 2
               _
       _      | |_   _
   _  | |_   _| | |_| |_
 _|_|W|_|_|W|_|_|_|_|_|_|
 0 1 2 3 4 5 6 7 8 9 A B
	--
	stacktop = 1 <1?
	push 1,6
	stack: [(1,6), (1,4), (2,3)]

	i: 7
	bar: 3
	stacktop = 1 < 3
	pop 1,6
	stack: [(1,4), (2,3)]
	top: 1,4
	left: 1, right = 3, groove = 1
	water trapped = min(1,1)-1 * (7-4-1) == 0*3 = 0
	Total water trapped += 0 = 2
	--
	stacktop = 1 < 3
	pop 1,4 => groove: 1
	stack: [(2,3)]
	top: 2,3 => left: 2
	water trapped = [min(2,3) - 1]* (7-3-1) == 1*3 = 3
	Total water trapped += 3 = 5
               _
       _      | |_   _
   _  | |W W W| | |_| |_
 _|_|W|_|_|W|_|_|_|_|_|_|
 0 1 2 3 4 5 6 7 8 9 A B
	--
	stacktop = 2 < 3
	pop 2,3
	top: NA
	stack: []
	push (3,7)
	stack: [(3,7)]

	i: 8
	bar: 2
	stacktop = 3 < 2? NO
	push (2,8)
	stack: [(2,8), (3,7)]

	i: 9
	bar: 1
	stacktop = 2 < 1? NO
	push (1,9)
	stack: [(1,9), (2,8), (3,7)]

	i: 10
	bar: 2
	stacktop = 1 < 2
	pop 1,9
	stack: [(2,8), (3,7)]
	top: 2,8 => left = 2
	groove = 1
	right = 2
	water trapped = [min(2,2)-1] * (10-8-1) == 1*1 = 1
	Total water trapped += 1 = 6
               _
       _      | |_   _
   _  | |W W W| | |W| |_
 _|_|W|_|_|W|_|_|_|_|_|_|
 0 1 2 3 4 5 6 7 8 9 A B

	--
	stacktop = 2 < 2? NO
	push (2,10)
	stack: [(2,10), (2,8), (3,7)]

	i: 11
	bar: 1
	stacktop == 2 <1? NO
	push (1,11)
	stack: [(1,11), (2,10), (2,8), (3,7)]

	i: 12 Out of array
	return 6
'''

class Stack:
	def __init__(self):
		self.items = []
	
	def __len__(self):
		return len(self.items)

	# unsafe, caller is expected to check if stack is empty
	# before calling
	def top(self):
		return self.items[-1]

	def push(self, x):
		self.items.append(x)

	# unsafe, caller is expected to check if stack is empty
	# before calling
	def pop(self):
		return self.items.pop()


class Solution(object):
	def trap(self, height):
		"""
		:type height: List[int]
		:rtype: int
		"""
		total_water_trapped = 0

		stack = Stack()
		for i in xrange(len(height)):
			while stack and height[stack.top()] < height[i]:
				groove = height[stack.pop()]
				if not stack:
					break

				top, idx = height[stack.top()], stack.top()
				water_trapped = (min(top, height[i]) - groove) * (i-idx-1)

				total_water_trapped += water_trapped

			stack.push(i)

		return total_water_trapped


if __name__ == '__main__':
	s = Solution()
	assert s.trap([]) == 0
	assert s.trap([2,1,2]) == 1
	assert s.trap([0,1,0,2,1,0,1,3,2,1,2,1]) == 6
	assert s.trap([2,1,2,6,9,7,5,5,7]) == 5
	assert s.trap([4,2,3]) == 1
	assert s.trap([3,2,1,0,1,5]) == 8
	assert s.trap([4,2,0,3,2,4,3,4]) == 10

