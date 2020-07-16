#encoding: utf-8
'''
https://www.interviewbit.com/problems/gas-station/

Gas Station

Given two integer arrays A and B of size N.
There are N gas stations along a circular route, where the amount of gas at station i is A[i].

You have a car with an unlimited gas tank and it costs B[i] of gas to travel from station i
to its next station (i+1). You begin the journey with an empty tank at one of the gas stations.

Return the minimum starting gas station’s index if you can travel around the circuit once, otherwise return -1.

You can only travel in one direction. i to i+1, i+2, … n-1, 0, 1, 2.. Completing the circuit means starting at i and
ending up at i again.


Input Format
The first argument given is the integer array A.
The second argument given is the integer array B.

Output Format
Return the minimum starting gas station's index if you can travel around the circuit once, otherwise return -1.

For Example
Input 1:
    A =  [1, 2]
    B =  [2, 1]
Output 1:
    1
Explanation 1:
	If you start from index 0, you can fill in A[0] = 1 amount of gas. Now your tank has 1 unit of gas. But you need B[0] = 2 gas to travel to station 1. 
	If you start from index 1, you can fill in A[1] = 2 amount of gas. Now your tank has 2 units of gas. You need B[1] = 1 gas to get to station 0. So, you travel to station 0 and still have 1 unit of gas left over. You fill in A[0] = 1 unit of additional gas, making your current gas = 2. It costs you B[0] = 2 to get to station 1, which you do and complete the circuit. 

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


