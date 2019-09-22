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
	5. Reduction rules:
		if stacktop, st < current bar, curr
		   pop stacktop
		   if the two bars are separated by distance 'd',
		   then max water that can be potentially stored between st and curr = min(st,curr)*d
		   We also keep track of the blocks in between by adding bar popped.
		   These blocks count the number of blocks in the current (un-reduced) window that cannot store water.
		   Actual water trapped = potential water storage - blocks - actual water trapped (so far)
			   e.g. [3,1,0,3]
			   between the two 3s, blocks = 1, poential =  2*3 == 6units
			   actual water trapped = 5 units
	    Once all bars < current are popped,
		    push current bar
			if the stack was empty before pushing current bar, reset blocks and water trapped in current reduction of the stack.

		   e.g.,
		   blocks = 0
		   stack: [2,1] and then followed by 2
		   1 <= 2, water storage between them is (2-1-1)*min(2,1) == 0
		   pop: 1
		   water = 0
		   blocks = 1
		   stack: [2]
		   Now 2 <= 2, water capacity between is (2-0-1)*2 == 2
		   pop: 2
		   minus 1 block => water trapped == 1
		   blocks = 2+1 == 3
		   stack is now empty, reset blocks = 0, total water = 1, curr water trapped = 0
		   push 2
		   stack: [2] <-- Everything until second 2 has been reduced, start a new 'window' beginning with it
		

Sample run 1:
=============
	   Bars: [3,2,1,0,1,5]

	   Stack: []
	   Total water = 0
	   current window water = 0
	   current window blocks = 0

	   i: 0
	   bar: 3
	   stack is empty
	   push 3,0
	   stack: [(3,0)]

	   i: 1
	   bar: 2
	   stacktop <= 2? NO
	   push 2,1
	   stack: [(2,1), (3,0)]

	   i: 2
	   bar: 1
	   stacktop <= 1? NO
	   stack: [(1,2), (2,1), (3,0)]

	   i: 3
	   bar: 0
	   stacktop <= 0? NO
	   stack: [(0,3), (1,2), (2,1), (3,0)]

	   i: 4
	   bar: 1
	   stacktop == 0 <= 1
	   pop: 0,3
	   stack: [(1,2), (2,1), (3,0)]
	   potential water storage = min(1,0)*(4-3-1) == 0
	   blocks += 0
	   --
	   stacktop == 1 <= 1
	   pop 1,2
	   stack: [(2,1), (3,0)]
	   potential water storage = min(1,1)*(4-2-1) == 1
	   Actual water = potential - blocks = 1-0 = 1
	   blocks += 1 == 1
	   --
	   stacktop == 2 <= 1? NO
	   push 1,4
	   stack: [(1,4), (2,1), (3,0)]

	   i: 5
	   bar: 5
	   stacktop = 1 <= 5
	   pop: 1,4
	   stack: [(2,1), (3,0)]
	   potential = min(5,1)*(5-4-1) == 1*0 == 0
	     Don't add to total water
	   blocks += 1 == 2
	   --
	   stacktop = 2 <= 5
	   pop: 2,1
	   stack: [(3,0)]
	   potential = min(2,5)*(5-1-1) == 2*3 == 6
	   actual += potential - blocks - actual == 6 - 2 - 1 == 3+1 == 4 
	   blocks += 2 == 4
	   --
	   stacktop = 3 <= 5
	   pop: 3,0
	   potential = min(3,5)*(5-0-1) == 3*4 == 12
	   actual = potential - blocks - actual == 12 - 4 - 4 == 4
	   actual += actual == 4+4 == 8
	   blocks += 3 == 7
	   --
	   stacktop -- empty
	   Add actual to total water  == +8
	   Total water: 8
	   reset actual = 0
	   blocks = 0
	   push 5,5

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
	current: 0

	i: 0
	bar: 0
	stacktop <=0? NO
	push (0,0)
	stack: [(0,0)]

	i: 1
	bar: 1
	remove <= 1
	pop: 0,0
	potential: min(1,0)*(1-0-1) == 0
	blocks += 0 = 0
	--
	stack: empty
	push (1,1)
	stack: [(1,1)]

	i: 2
	bar: 0
	stacktop <= 0? NO
	push (0,2)
	stack: [(0,2), (1,1)]

	i: 3
	bar: 2
	stacktop = 0 <= 2
	pop: 0,2
	potential: min(2,0)*(3-2-1) == 0
	blocks += 0 = 0
	stack: [(1,1)]
	--
	stacktop = 1 <= 2
	pop: 1,1
	potential: min(2,1)*(3-1-1) == 1*1 == 1
	current: potential - blocks - current = 1 - 0 - 0 = 1
	blocks += 1 == 1
	stack: []
	--
	pop: empty
	total water += current = 1
	current: 0
	blocks: 0
	push (2,3)
	stack: [(2,3)]

	i: 4
	bar: 1
	stacktop <= 1? NO
	push (1,4)
	stack: [(1,4), (2,3)]

	i: 5
	bar: 0
	stacktop <= 0? NO
	push (0,5)
	stack: [(0,5), (1,4), (2,3)]

	i: 6
	bar: 1
	stacktop = 0 <= 1
	pop: 0,5
	potential: min(0,1)*(6-5-1) == 0
	blocks += 0 == 0
	stack: [(1,4), (2,3)]
	--
	stacktop = 1 <=1
	pop: 1,4
	potential: min(1,1)*(6-4-1) == 1
	current += potential - blocks - current = (1-0-0) = 1
	blocks += 1 == 1
	stack: [(2,3)]
	--
	stacktop = 2 <=1? NO
	push (1,6)
	stack: [(1,6),(2,3)]


	i: 7
	bar: 3
	stacktop = 1 <= 3
	pop 1,6
	potential = min(3,1)*(7-6-1)==0
	blocks += 1 = 2
	stack: [(2,3)]
	--
	stacktop = 2 <= 3
	pop 2,3
	potential = min(3,2)*(7-3-1) = 2*3 == 6
	current += potential - blocks - current = (6-2-1) == 3+1 == 4
	blocks += 3 == 5
	stack: []
	--
	stack empty
	Total water: 1+4 == 5
	current: 0
	blocks: 0
	push (3,7)
	stack: [(3,7)]

	i: 8
	bar: 2
	stacktop = 3 <= 2? NO
	push (2,8)
	stack: [(2,8), (3,7)]

	i: 9
	bar: 1
	stacktop = 2 <= 1? NO
	push (1,9)
	stack: [(1,9), (2,8), (3,7)]

	i: 10
	bar: 2
	stacktop = 1 <= 2
	pop 1,9
	potential: min(2,1)*(10-9-1) == 0
	blocks += 1 = 1
	stack: [(2,8), (3,7)]
	--
	stacktop = 2 <= 2
	pop 2,8
	potential: min(2,2)*(10-8-1) == 2*1 == 2
	current += potential - blocks - current = (2-1-0) = 1
	blocks += 2 == 3
	stack: [(3,7)]
	--
	stacktop = 3 <= 2? NO
	push (2,10)
	stack: [(2,10), (3,7)]

	i: 11
	bar: 1
	stacktop == 2 <=1? NO
	push (1,11)
	stack: [(1,11), (2,10), (3,7)]

	i: 12 Out of array
	outstanding current? == 1
	Total water: 5+1 == 6
	return 6
'''
