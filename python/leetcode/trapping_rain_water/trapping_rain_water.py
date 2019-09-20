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
	1. The problem is similar to the parantheses matching problem except different bar heights are like parantheses, bars with same heights can be considered the opening and closing parantheses of the same type.
	2. Use a stack to store different bar heights while ensuring that at no point in the stack, a smaller bar height is beneath  a higher one.
	   WHY?
	   because [1,2,3] is equivalent [3] as there's nothing to hold to the left.
	   As does [3,4,2,1] => [4,2,1]
	3. Keep track of both the bar height, and its position from the left so we can calculate horizontal distances between any two bars, and the water-holding capacity.	
	   [4.0,0,4] -> holds 4 2x4 == 8 units of water
	   [4,0,4] -> holds 4 units of water.
	4. To keep the stack ordered by bar height (and that the values are always decreasing from the bottom of the stack to the top), we 'reduce' the stack anytime a higher value than stacktop is found.
	   Reduce by popping all bars (< current bar height) from the top until the new bar can be added, while calculating the water that can be stored.
	   If the stack becomes empty, then we restart water storage starting from the current bar, while adding the previous trappings to the total count.

	   e.g,
	   Bars: [3,2,1,0,1,5]

	   After processing [3,2,1,0]
		stack: [(3,0), (2,1), (1,2), (0,3)]
		next bar: 1
		we pop everything <=1
		which are [(1,2), (0,3)] -> this represents [1,0,1] with current storage to 1, [1,1] are the two blocks inbetween, 
		l == r == 1, => count both blocks
		so blocks = 2
		water: 1, blocks: 2
		 _        
		| |_       
		| | |_   _
		|_|_|_|W|_|

		stack: [(3,0),(2,1)]
		next bar: 4
		pop everything <= 4
		pop: 2,1
		this would be [(3,0), (2,1), (4,5)] --> 2 and 4 are separated by 3 blocks horizontally with 1 unit of water + 2 blocks -> this represents row 1
		Capacity between 2 and 4 is min(2,4)*(5-1-1) == 2*3 == 6, of these 3 are already accounted for.
		=> new water storage is 3+1 == 4, blocks = 2+2 == 4
		           _
		 _        | |
		| |_      | |
		| | |W W W| |
		|_|_|_|W|_|_|

		pop: 3,0
		this would be [(3,0), (4,5)]
		capacity between (3,0) and (4,5) is (5-0-1)*min(3,4) == 4*3 == 12
		of these 4W + 4 blocks are occupied.
		leaving 4w to be filled.
		=> water: 8, blocks: 3+4 == 7
                   _
		 _        | |
		| |W W W W| |
		| | |W W W| |
		|_|_|_|W|_|_|

		pop(): empty
		4 can now be pushed, as everything to its left is 'reduced'.
		water: 0, blocks: 0
		total water: 8


Sample run:
	Input: [0,1,0,2,1,0,1,3,2,1,2,1]
	stack: []
	Total water: 0

	i: 0
	bar: 0
	push (0,0)
	stack: [(0,0)]

	i: 1
	bar: 1
	remove <= 1
	pop: 0,0
	capacity: min(1,0)*(1-0-1) == 0
	stack: empty
	water: 0, blocks: 0
	push (1,1)
	stack: [(1,1)]

	i: 2
	bar: 0
	remove <= 0
	pop: []
	push (0,2)
	stack: [(0,2), (1,1)]

	i: 3
	bar: 2
	remove <= 2
	pop: 0,2
	capacity: min(2,0)*(3-2-1) == 0
	pop: 1,1
	capacity: min(2,1)*(3-1-1) == 1*1 == 1
	blocks: 1*2 == 2
	water: 1
	pop: empty
	total water: 1
	blocks: 0
	push (2,3)
	stack: [(2,3)]

	i: 4
	bar: 1
	remove <=1
	push (1,4)
	stack: [(1,4), (2,3)]

	i: 5
	bar: 0
	remove <=0
	push (0,5)
	stack: [(0,5), (1,4), (2,3)]

	i: 6
	bar: 1
	remove <=1
	pop: 0,5
	capacity: min(0,1)*(6-5-1) == 0
	pop: 1,4
	capacity: min(1,1)*(6-4-1) == 1
	water: 1, blocks: 2
	total water: 2
	push? (no push)
	stack: [(2,3)]

	i: 7
	bar: 3
	remove <= 3
	pop: (2,3)
	capacity: min(2,3)*(7-3-1) == 2*3 ==6
	water: 6 - water - blocks = 6-1-2 == 3
	total water: 5
	stack: empty
	water:0, blocks: 0
	push (3, 7)

	i: 8
	bar : 2
	push (2,8)
	stack: [(2,8), (3,7)]

	i: 9
	bar: 1
	push (1,9)
	stack: [(1,9), (2,8), (3,7)]

	i: 10
	bar: 2
	pop: (1,9)
	capacity: min(2,1)*(10-9-1) == 0
	blocks = 1
	pop: (2,8)
	l == r
	capacity: min(2,2)*(10-8-1) == 2*1 == 2
	water: 2 - water - blocks == 2 - 0 - 1 == 1
	total water: 5+1 == 6
	l == r, dont push, blocks += 2*2+1 == 5
	stack: [(3,7)]

	i: 11
	bar: 1
	push (1,11)
	stack: [(1,11), (3,7)]

	return 6
'''
