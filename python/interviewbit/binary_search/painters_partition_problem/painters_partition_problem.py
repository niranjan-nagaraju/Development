#encoding: utf-8
'''
https://www.interviewbit.com/problems/painters-partition-problem/

Painter's Partition Problem

Given 2 integers A and B and an array of integers C of size N.

Element C[i] represents length of ith board.

You have to paint all N boards [C0, C1, C2, C3 … CN-1]. There are A painters available and each of them takes B units of time to paint 1 unit of board.

Calculate and return minimum time required to paint all boards under the constraints that any painter will only paint contiguous sections of board.
	1. 2 painters cannot share a board to paint. That is to say, a board
	cannot be painted partially by one painter, and partially by another.
	2. A painter will only paint contiguous boards. Which means a
	configuration where painter 1 paints board 1 and 3 but not 2 is
	invalid.
Return the ans % 10000003

Input Format
The first argument given is the integer A.
The second argument given is the integer B.
The third argument given is the integer array C.

Output Format
Return minimum time required to paint all boards under the constraints that any painter will only paint contiguous sections of board % 10000003.

Constraints
	1 <= A <= 1000
	1 <= B <= 10^6
	1 <= C.size() <= 10^5
	1 <= C[i] <= 10^6

For Example
Input 1:
    A = 2
    B = 5
    C = [1, 10]
Output 1:
    50
Explanation 1:
    Possibility 1:- same painter paints both blocks, time taken = 55units
    Possibility 2:- Painter 1 paints block 1, painter 2 paints block 2, time take = max(5, 50) = 50
    There are no other distinct ways to paint boards.
    ans = 50%10000003

Input 2:
    A = 10
    B = 1
    C = [1, 8, 11, 3]
Output 2:
    11
'''

'''
Solution Outline:
	Reference: https://www.topcoder.com/community/competitive-programming/tutorials/binary-search/

	Given A painters, C[N] boards,
	  The lowest max (sections of boards) that any painter can paint is max(C) {even when we assign atmost 1 board to a painter}
	  The highest max (sections of boards) that any painter can paint is sum(C) {when a single painter paints all boards}
	  The optimal solution is somewhere in between.

	  NOTE: As we move left towards lowest max, number of painters needed increases
	        and conversely, as we move right, number of painters needed decreases

	  Use binary search to find a 'max' allocation between {lowest_max, highest_max} with an optimal number of painters.
        => upper bound(max, # painters being minimum)

	  Time taken to finish painting all the boards = time taken for the painter with max sections to finish his portion of the work.
	  => (upper_bound * B)
	  return (upper_bound * B) % 10000003

	  NOTE: if A >= len(C) => As many as len(C) workers can each be assigned 1 board to paint, and max sections for any painter = max(C)

Sample run:
	A: 10
	B: 1
	C: [1,8,11,3]

	l = lowest max = max(C) == 11
	h = highest max = sum(C) == 23
	mid = (l+h)/2 == (11+23)/2 == 17
	Allocation of painters with utmost 17 sections for each is 2
	   Painter #1: [1,8] = 9
	   Painter #2: [11,3] = 14
	2 <= 10 => search left for a better 'max' with more number of painters
	h = mid = 17

	l = 11, h = 17
	mid = (l+h)/2 == 14
	Allocation of painters with utmost 14 sections for each is 2
	   Painter #1: [1,8] = 9
	   Painter #2: [11,3] = 14
	2 <= 10 => search left for a better 'max' with more number of painters
	h = mid = 14

	l = 11, h = 14
	mid = (l+h)/2 == 12
	Allocation of painters with utmost 12 sections for each is 3
	   Painter #1: [1,8] = 9
	   Painter #2: [11] = 11
	   Painter #3: [3] = 3
	3 <= 10 => search left for a better 'max' with more number of painters
	h = mid = 12

	l = 11, h = 12
	mid = (l+h)/2 == 11
	Allocation of painters with utmost 11 sections for each is 3
	   Painter #1: [1,8] = 9
	   Painter #2: [11] = 11
	   Painter #3: [3] = 3
	3 <= 10 => search left for a better 'max' with more number of painters
	h = mid = 11

	l=h, => l is max sections any painter paints
	= 11

	This also is the time it takes to complete painting all the boards (the one with the most sections finishes last)
	=> time taken = 11 * B == 11
'''
class Solution:
	def painters_partition(self, A, B, C):
		# Find number of painters needed
		# to paint all C boards
		# with each painter painting utmost 'max_sections'
		def find_allocation(max_sections):
			num_painters = 1
			current_sections = 0
			for x in C:
				if x+current_sections > max_sections:
					current_sections = 0
					num_painters += 1

				current_sections += x
			return num_painters

		mod = 10000003
		l = max(C)

		# There are enough painters to handle a board each
		# max time taken = max sections in any board * time taken to paint a section
		if A >= len(C):
			return (l*B) % mod

		h = sum(C)
		while l < h:
			mid = l + (h-l)/2 # incase (l+h)/2 overflows
			painters = find_allocation(mid)
			if painters <= A:
				# Look to the left to see if
				# we can find a better upper-bound
				h = mid
			else:
				# painters > A
				# Look to the right to reduce
				# number of painters needed
				l = mid+1

		return (l*B) % mod




if __name__ == '__main__':
	s = Solution()
	assert s.painters_partition(2, 10, [12, 34, 67, 90]) == 113*10
	assert s.painters_partition(2, 5, [1, 10]) == 50
	assert s.painters_partition(10, 1, [1, 8, 11, 3]) == 11
	assert s.painters_partition(2, 5, [10, 20, 30, 40]) == 60*5

