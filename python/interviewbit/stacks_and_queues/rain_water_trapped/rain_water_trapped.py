'''
https://www.interviewbit.com/problems/rain-water-trapped/

Rain Water Trapped

Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.

Input Format
The only argument given is integer array A.

Output Format
Return the total water it is able to trap after raining..

For Example
Input 1:
    A = [0,1,0,2,1,0,1,3,2,1,2,1]
Output 1:
    6

Explaination 1:
    In this case, 6 units of rain water (blue section) are being trapped.
              _
      _      | |_   _   _
  _  | |_   _| | |_| |_| |_
_|_|_|_|_|_|_|_|_|_|_|_|_|_|
'''


'''
Solution Outline:
	3 blocks are needed to trap water.
	A left block, a middle groove (with height < left) and a right block (with height >= left)
	Use a stack to keep track of all left, groove blocks (ie the stacktop is always the current minima)
	When a right block (> stacktop) is encountered, flush groove and left and calculate water that can be stored in them.
	  == (min(left, right) * (ri-li) - groove

	Once, a groove is accounted for, its water trapping capacity can now be counted as a block.
	for e.g,
	[3,2,1,2,3]
	[2,1,2] has a groove [1] between and traps 1 units of water.
	Once we calculate this, we can now mark [2,1,2] as [2,2,2]
	and [3,2,2,2,3] can now trap [3] units of water, and in total, 4 units of water.
'''

class Stack:
	def __init__(self):
		self.items = []
	
	def __len__(self):
		return len(self.items)

	def top(self):
		return self.items[-1] if self.items else None

	def push(self, x):
		self.items.append(x)

	def pop(self):
		return self.items.pop() if self.items else None


class Solution:
	def trapWater(self, bars):
		stack = Stack()
		total_water_trapped = 0

		for i in xrange(len(bars)):
			# Reduce all bars in the stack as long as current bar is bigger
			while stack and stack.top()[0] < bars[i]:
				groove,_ = stack.pop()

				# Could not find a left bar
				# For e.g, [2, 3]
				# stack: [2], bars[i] = 3
				if not stack:
					break

				left, lidx = stack.top()

				# water trapped = min(bars[i], left) - groove) * (i - lidx - 1)
				# for e.g., [(4,1), (3,2), (5,3)] can trap:
				#    min(4,5) == 4
				#    water trapped = (4 - 3) * (3-1-1) == 1*1 == 1 units of water
				water_trapped = (min(bars[i], left) - groove) * (i - lidx - 1)
				total_water_trapped += water_trapped

			stack.push((bars[i], i))

		return total_water_trapped
		

if __name__ == '__main__':
	s = Solution()
	assert s.trapWater([0,1,0,2,1,0,1,3,2,1,2,1]) == 6
	assert s.trapWater([3,2,1,2,3]) == 4
	assert s.trapWater([4,2,3]) == 1

