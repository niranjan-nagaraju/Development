'''
https://leetcode.com/problems/gas-station/

134. Gas Station

here are N gas stations along a circular route, where the amount of gas at station i is gas[i].

You have a car with an unlimited gas tank and it costs cost[i] of gas to travel from station i to its next station (i+1). You begin the journey with an empty tank at one of the gas stations.

Return the starting gas station's index if you can travel around the circuit once in the clockwise direction, otherwise return -1.

Note:
If there exists a solution, it is guaranteed to be unique.
Both input arrays are non-empty and have the same length.
Each element in the input arrays is a non-negative integer.

Example 1:
Input: 
gas  = [1,2,3,4,5]
cost = [3,4,5,1,2]

Output: 3

Explanation:
Start at station 3 (index 3) and fill up with 4 unit of gas. Your tank = 0 + 4 = 4
Travel to station 4. Your tank = 4 - 1 + 5 = 8
Travel to station 0. Your tank = 8 - 2 + 1 = 7
Travel to station 1. Your tank = 7 - 3 + 2 = 6
Travel to station 2. Your tank = 6 - 4 + 3 = 5
Travel to station 3. The cost is 5. Your gas is just enough to travel back to station 3.
Therefore, return 3 as the starting index.


Example 2:

Input: 
gas  = [2,3,4]
cost = [3,4,3]

Output: -1

Explanation:
You can't start at station 0 or 1, as there is not enough gas to travel to the next station.
Let's start at station 2 and fill up with 4 unit of gas. Your tank = 0 + 4 = 4
Travel to station 0. Your tank = 4 - 3 + 2 = 3
Travel to station 1. Your tank = 3 - 3 + 3 = 3
You cannot travel back to station 2, as it requires 4 unit of gas but you only have 3.
Therefore, you can't travel around the circuit once no matter where you start.
'''


'''
Solution Outline:
	Start at Station 0, Fill gas[0], and get to station 1 with cost[0], and so on.
	if at anytime, gas[i] - cost[i] drives the tank to -ve, then our chosen start point won't work.
	Pick station i+1 as the starting point and retry.
	  Return last-saved starting point at the end of the pass.

	NOTE: sum(gas) >= sum(cost) => there exists a solution or else we'd be 'spending more than we can earn'

Sample Input 1:
	gas:  [1,2,3,4,5]
	cost: [3,4,5,1,2]

	sum(gas) = sum(cost) => solution exists

	i: 0
	  tank: 0
	  tank += gas[0] - cost[0] == 1-3 < 0
	    try starting at 1
		start = 1, tank = 0
	i: 1
	  tank: 0
	  tank += gas[1] - cost[1] == (2-4) == -2
		start = 2, tank = 0

	i: 2
	  tank: 0
	  tank += gas[2] - cost[2] == (3-5) == -2
		start = 3, tank = 0

	i: 3
	  tank: 0
	  tank += gas[3] - cost[3] == (4-1) == 3
	
	i: 4
	  tank: 3
	  tank += gas[4] - cost[4] == (5-2) == 3 == 6

	return start = 3
	verify:
	   [3]: +4 -1 == 3
	   [4]: +5 -2 == 6
	   [0]: +1 -3 == 4
	   [1]: +2 -4 == 2
	   [2]: +3 -5 == 0, back at [3]


Sample Input 2:
	gas:  [1,2,3,4,5]
	cost: [1,5,2,4,3]
	start: 0

	i: 0
	  tank: 0
	  tank += gas[0] - cost[0] == 0

	i: 1
	  tank: 0
	  tank += gas[1] - cost[1] == (2-5) < 0
	    start = 2, tank = 0
	
	i: 2
	  tank: 0
	  tank += gas[2] - cost[2] == (3-2) == 1

	i: 3
	  tank: 1
	  tank += gas[3] - cost[3] == (4-4) == 1

	i: 4
	  tank: 1
	  tank += gas[4] - cost[4] == (5-3) == 3

   return last-stored
      start = 2

  verify:
    [2]: +3 -2 == 1
	[3]: +4 -4 == 1
	[4]: +5 -3 == 3
	[0]: +1 -1 == 3
	[1]: +2 -5 == 0
	[2]: back to [2]
'''

class Solution(object):
	def canCompleteCircuit(self, gas, cost):
		"""
		:type gas: List[int]
		:type cost: List[int]
		:rtype: int
		"""

		# Available gas < gas needed
		# No solution exists
		if sum(gas) < sum(cost):
			return -1

		tank, start = 0, 0
		for i in xrange(len(gas)):
			tank += gas[i] - cost[i]

			# Tank capacity dips to -ve from current start station,
			# Use the next station as possible candidate
			if tank < 0:
				tank, start = 0, i+1

		# return last-saved start
		return start



if __name__ == '__main__':
	s = Solution()
	assert s.canCompleteCircuit([1,2], [2,1]) == 1
	assert s.canCompleteCircuit([1,2,3,4,5], [3,4,5,1,2]) == 3
	assert s.canCompleteCircuit([1,2,3,4,5], [1,5,2,4,3]) == 2
	assert s.canCompleteCircuit([2,3,4], [3,4,3]) == -1
	assert s.canCompleteCircuit([1,2,3,4,5], [1,2,3,4,5]) == 0

        
