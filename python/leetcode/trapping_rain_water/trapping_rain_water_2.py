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

               _                                                                                                                                            
       _      | |_   _
   _  | |_   _| | |_| |_
 _|_|_|_|_|_|_|_|_|_|_|_|

[0,1,0,2,1,0,1,3,2,1,2,1]

Solution Outline: O(nh), h: max bar height, n: number of bars
	Think of the bars as a 2d matrix of blocks, a full column representing a bar, individual rows representing number of bars.
	Each cell (i,j) in the matrix is either 1 if the bar i has enough height j, 0 otherwise.
	So [2,1,2] represents a matrix of 2x3 like below
	1 0 1
	1 1 1

	Visually:
     _   _
    | |_| |
    |_|_|_|
	
	With such a representation, water filling the gaps can be simulated line-by-line bottom-up (think a line-printer going bottom to up)
	Count all the 'empty' slots in between, ignore all trailing and leading slots (as these cannot hold water)

	NOTE: We don't actually need to create a grid representing the matrix of 1s and 0s,
		  Just decrement bar height at every level, -ve values means gaps


Sample run:
==========

Elevation map:
[0,1,0,2,1,0,1,3,2,1,2,1]
               _                                                                                                                                            
       _      | |_   _
   _  | |_   _| | |_| |_
 _|_|_|_|_|_|_|_|_|_|_|_|

Level 1:
	Trim left gaps
	[1,0,2,1,0,1,3,2,1,2,1]
	Trim right gaps: None
	Count number of gaps: 2

Level 2:
	[0,-1,1,0,-1,0,2,1,0,1,0]
	Trim left gaps
	[1,0,-1,0,2,1,0,1,0]
	Trim right gaps
	[1,0,-1,0,2,1,0,1]
	gaps: [0, -1, 0, 0] == 4
	
Level 3:
	[0,-1,-2,-1,1,0,-1,0]
	Trim left gaps
	[1,0,-1,0]
	Trim right gaps
	[1]
	gaps: 0

Level 4:
	[]

Total water trapped: 2+4 == 6
'''

class Solution(object):
	def trap(self, height):
		"""
		:type height: List[int]
		:rtype: int
		"""
		total_water_trapped = 0

		# lb, ub represents the active part of the bars elevation maps
		# that are still under consideration after all the trims
		lb = 0
		rb = len(height)-1
		while True:
			water_in_curr_level = 0
			# trim left gaps
			i = lb
			while i < len(height) and height[i] <= 0:
				i += 1
			lb = i

			# trim right gaps
			i = rb
			while i >= 0 and height[i] <= 0:
				i -= 1
			rb = i

			# processed all bar heights
			if not height[lb:rb+1]:
				break

			water_in_curr_level = reduce(lambda x,y: x+1 if y <= 0 else x, height[lb:rb+1], 0)

			total_water_trapped += water_in_curr_level

			# decrement all bar heights to move to next level
			for i in xrange(lb, rb+1):
				height[i] -= 1

		return total_water_trapped


if __name__ == '__main__':
	s = Solution()
	assert s.trap([]) == 0
	assert s.trap([0,1,0,2,1,0,1,3,2,1,2,1]) == 6
	assert s.trap([3,2,1,0,1,5]) == 8
	assert s.trap([4,2,3]) == 1
	assert s.trap([2,1,2,6,9,7,5,5,7]) == 5

