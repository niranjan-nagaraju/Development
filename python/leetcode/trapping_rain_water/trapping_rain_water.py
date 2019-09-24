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
		 Calculate water trapped by discounting the blocks, and using a height = min(left_anchor, right_anchor)
	   If the stack becomes empty, then we restart water storage starting from the current bar, using it as the left anchor
	5. Reduction rules:
		Until stacktop, st < current bar, curr
		   pop stacktop -- These are the grooves,
		   We keep track of the blocks in between by adding bar popped.
		   At this point, stacktop >= popped bar, Add popped bar height to blocks
		   These blocks count the number of blocks in the current (un-reduced) window that cannot store water.
		   if the two bars (stacktop, current bar) are separated by distance 'd',
		   then max water that can be potentially stored between st and curr = min(st,curr)*d
		   Actual water trapped = potential water storage - blocks
		   Total water trapped += actual water trapped
		   Add actual water to 'blocks' - This is because once water has been trapped in a groove,
		   we can discount that groove in further calculations.
			  e.g.
			   [3,2,1,2]
			   [2,1,2] can be reduced to 1 units of water, and effectively be considered [2,2,2] after
			   so if [3,2,1,2] is followed by a [3]
			   then, [3,2,1,2,3] => [3,2,2,2,3] == 3 units of water trapped between [3..3]
			   effectively bringing the total water trapped between [3,2,1,2,3] to 4
			   --
			   e.g. [3,1,4]
			   between the 3 and 4, blocks = 1, potential =  2*3 == 6units
			   actual water trapped = 5 units
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
			   blocks += 1 = 1
			   stack: [2,3]
			   top: 2
			   left: 2, right: 4, blocks in between = 1
			   actual water trapped between 2 and 4 is (2)*(1) - 1 = 1 units
			   Total water trapped += 1 == 1
			   blocks += 1 == 2
			   > pop 2
			   blocks += 2 == 4
			   stack: [3]
			   top: 3
			   left: 3, right: 4, blocks in between = 4
			   actual water trapped between 3 and 4 is (3)*(2) - 4 = 2 units
			   Total water trapped += 2 == 3
			   blocks += 2 == 6
			   > pop 3
			   blocks += 3 == 9
			   stack: []
			   top: NA
			   left: ?, right: 4, blocks in between = 9
			   stack: empty, push (4)
	    Once all bars < current are popped,
		    push current bar
			if the stack was empty before pushing current bar, reset blocks and water trapped in current reduction of the stack.

		   e.g.,
		   blocks = 0
		   bar: [2,1,3]
		   At some stage, stack: [1,2], current: 3
		   1 < 3, 
		   pop: 1
		   blocks += 1 == 1
		   stack: [2]
		   potential water storage between curr,stacktop is (2-0-1)*min(2,3) == 2
		   water = potential - blocks = 2-1 = 1
		   blocks += 1 == 2
		   pop: 2
		   stack: []
		   stack is now empty, reset blocks = 0, total water = 1, curr water trapped = 0
		   push 3
		   stack: [3] <-- Everything until 3 has been reduced, start a new 'window' anchored at bar 3
		

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
	   current window blocks = 0

	   i: 0
	   bar: 3
	   stack is empty
	   push 3,0
	   stack: [(3,0)]
		_ 
	   | |
	   | |
	   |_|

	   i: 1
	   bar: 2
	   stacktop < 2? NO
	   push 2,1
	   stack: [(2,1), (3,0)]
		_   
	   | |_ 
	   | | |
	   |_|_|

	   i: 2
	   bar: 1
	   stacktop < 1? NO
	   stack: [(1,2), (2,1), (3,0)]
		_   
	   | |_ 
	   | | |_
	   |_|_|_|

	   i: 3
	   bar: 0
	   stacktop < 0? NO
	   stack: [(0,3), (1,2), (2,1), (3,0)]
		_   
	   | |_ 
	   | | |_
	   |_|_|_|_

	   i: 4
	   bar: 1
	   stacktop == 0 < 1
	   pop: 0,3
	   blocks += 0 = 0
	   stack: [(1,2), (2,1), (3,0)]
	   top: 1,2
	   water trapped = min(1,1)*(4-2-1) - blocks == 1
	   Total water trapped += 1 = 1
	   blocks += 1 = 1
		_   
	   | |_ 
	   | | |_
	   |_|_|_|_
	   --
	   stacktop == 1 < 1
	   pop 1,2
	   blocks += 1 = 2
	   stack: [(2,1), (3,0)]
	   top: 2,1
	   water trapped = min(2,1)*(4-1-1) - blocks == 1*2-2 = 0
	   Total water trapped += 0 = 1
	   blocks += 0 == 2
		_   
	   | |_ 
	   | | |_
	   |_|_|_|_
	   --
	   stacktop == 2 < 1? NO
	   push 1,4
	   stack: [(1,4), (2,1), (3,0)]
                  _ 
		_        | | 
	   | |_      | |
	   | | |_   _| |
	   |_|_|_|W|_|_|

	   i: 4
	   bar: 5
	   stacktop = 1 < 4
	   pop: 1,4
	   blocks += 1 = 3
	   stack: [(2,1), (3,0)]
	   top: 2,1
	   water trapped = min(4,2)*(5-1-1) - blocks == 2*3 - 3 == 3
	   blocks += 3 == 6
	   Total water trapped += 3 = 4
                  _ 
		_        | | 
	   | |_      | |
	   | | |W W W| |
	   |_|_|_|W|_|_|

	   --
	   stacktop = 2 < 4
	   pop: 2,1
	   blocks += 2 = 8
	   stack: [(3,0)]
	   top: 3,0
	   water trapped = min(3,4)*(5-0-1) - blocks = 3*4 - 8 == 4
	   blocks += 4 == 12
	   Total water trapped += 4 == 8
                  _ 
		_        | | 
	   | |W W W W| |
	   | | |W W W| |
	   |_|_|_|W|_|_|

	   --
	   stacktop = 3 < 4
	   pop: 3,0
	   top: []
	   stack: []
	   blocks = 0
	   push 4,5
                   _
		 _        | |
		| |W W W W| |
		| | |W W W| |
		|_|_|_|W|_|_|

		total water: 8


Sample run 2:
============
	Input: [0,1,0,2,1,0,1,3,2,1,2,1]
	Index:  0 1 2 3 4 5 6 7 8 9 10 11
	stack: []
	Total water: 0
	blocks: 0

	i: 0
	bar: 0
	stacktop <0? NO
	push (0,0)
	stack: [(0,0)]

	i: 1
	bar: 1
	stacktop < 1
	pop: 0,0
	blocks += 0 = 0
	stack: []
	top: NA
	blocks = 0 = 0
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
	blocks += 0 = 0
	top: 1,1
	water trapped = min(1,2) * (3-1-1) - blocks == 1*1 - 0 = 1
	blocks += 1 = 1
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
	reset, blocks = 0
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
	blocks +=0 = 0
	stack: [(1,4), (2,3)]
	top: 1,4
	water trapped = min(1,1) * (6-4-1) - blocks == 1*1 - 0 = 1
	blocks += 1 = 1
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
	blocks += 1 = 2
	stack: [(1,4), (2,3)]
	top: 1,4
	water trapped = min(1,3) * (7-4-1) - blocks == 1*2 - 2 = 0
	Total water trapped += 0 = 2
	blocks += 0 = 2
	--
	stacktop = 1 < 3
	pop 1,4
	blocks += 1 = 3
	stack: [(2,3)]
	top: 2,3
	water trapped = min(2,3) * (7-3-1) - blocks == 2*3 - 3 = 3
	blocks += 3 = 6
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
	blocks = 0
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
	top: 2,8
	blocks += 1 == 1
	water trapped = min(2,2) * (10-8-1) - blocks == 2*1 - 1 = 1
	blocks += 1 = 2
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
class Solution(object):
	def trap(self, height):
		"""
		:type height: List[int]
		:rtype: int
		"""
		total_water_trapped = 0

		blocks = 0
		stack = []
		for i in xrange(len(height)):
			print 'Current:', height[i], i
			while stack and stack[0][0] < height[i]:
				print stack
				curr_bar,_ = stack.pop(0)

				print 'Popped:', curr_bar
				
				if not stack:
					break

				blocks += curr_bar
				top, idx = stack[0]
				water_trapped = min(top, height[i]) * (i-idx-1) - blocks

				print 'Top:', top, idx
				print 'Blocks after pop:', blocks
				print 'Water trapped between', (height[i], i), 'and', (top, idx), 'is:', water_trapped

				total_water_trapped += water_trapped
				blocks += water_trapped
				print 'Blocks after water:', blocks


			# stack is empty
			if not stack:
				# reset current window
				# as height[i] will be the new left anchor
				# and everything to the left of height[i] has been reduced
				blocks = 0

			stack.insert(0,(height[i], i))

		return total_water_trapped


if __name__ == '__main__':
	s = Solution()
	assert s.trap([]) == 0
	assert s.trap([2,1,2]) == 1
	assert s.trap([0,1,0,2,1,0,1,3,2,1,2,1]) == 6
	assert s.trap([2,1,2,6,9,7,5,5,7]) == 5
	assert s.trap([4,2,3]) == 1
	assert s.trap([4,2,0,3,2,4,3,4]) == 10 # FIXME: actual returned value is -6
