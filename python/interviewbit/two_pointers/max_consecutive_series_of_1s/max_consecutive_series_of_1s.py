'''
https://www.interviewbit.com/problems/max-continuous-series-of-1s/

Max Continuous Series of 1s

You are given with an array of 1s and 0s. And you are given with an integer M, which signifies number of flips allowed.
Find the position of zeros which when flipped will produce maximum continuous series of 1s.

For this problem, return the indices of maximum continuous series of 1s in order.

Example:
Input : 
Array = {1 1 0 1 1 0 0 1 1 1 },  M = 1
Output : 
[0, 1, 2, 3, 4]

Example:
Input : 
Array = {0 0 1 1 0 0 1 1 1 0 1 1 0 0 0 1 1 1},  M = 3
Output : 
[2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

If there are multiple possible solutions, return the sequence which has the minimum start index.
'''

'''
Solution Outline:
	1. Use a sliding window s.t. the number of 0s inside a window does not exceed M
	2. Keep expanding the window to the right as long as the number of zeroes don't exceed M
	   2.1 If adding a new 0 exceeds M, shrink start-of-window to exclude the first 0 in the current window
	3. Return the maximum window (by size) at any given time.

Sample run:
Array = {0 0 1 1 0 0 1 1 1 0 1  1  0  0  0  1  1  1},  M = 3
		 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17

i: 0
window: [0] (0,0)

i: 1
window: [0 0] (0,1)

i: 2
window: [0 0 1] (0,2)

i: 3
window: [0 0 1 1] (0,3)

i: 4
window: [0 0 1 1 0] (0,4)

i: 5
window: [0 0 1 1 0 0] (0,5) m == 4 > 3
max-window-size = 5
shrink-window: [0 1 1 0 0] (1,5)

i: 6
window: [0 1 1 0 0 1] (1,6)

i: 7
window: [0 1 1 0 0 1 1] (1,7)

i: 8
window: [0 1 1 0 0 1 1 1] (1,8)

i: 9
window: [0 1 1 0 0 1 1 1 0] (1,9) m == 4 > 3
max-window-size = 8
shrink-window: [1 1 0 0 1 1 1 0] (2,9)

i: 10
window: [1 1 0 0 1 1 1 0 1] (2,10)

i: 11
window: [1 1 0 0 1 1 1 0 1 1] (2,11)

i: 12
window: [1 1 0 0 1 1 1 0 1 1 0] (2,12) m = 4 > 3
max-window-size = 10
shrink-window: [0 1 1 1 0 1 1 0] (5,12)

i: 13
window: [0 1 1 1 0 1 1 0 0] (5,13) m = 4 > 3
max-window-size = 10
shrink-window: [1 1 1 0 1 1 0 0] (6,13)

i: 14
window: [1 1 1 0 1 1 0 0 0] (6,14) m = 4 > 3
max-window-size = 10
shrink-window: [1 1 0 0 0] (10,14)

i: 15
window: [1 1 0 0 0 1] (10,15)

i: 16
window: [1 1 0 0 0 1 1] (10,16)

i: 17
window: [1 1 0 0 0 1 1 1] (10,17)
window-size: 8
EOF

max-window-size: 10
[2..11]
return [2,3,4,5,6,7,8,9,10,11]
'''
class Solution:
	def find_max_consecutives_1s(self, A, M):
		sow = 0 # start of window
		eow = -1 # end of window
		
		max_sow = max_eow = 0
		for x in A:
			eow += 1
			if x == 0:
				if M == 0:
					# Ran out of 0s to add in the current window
					# Check if max window (excluding newly added 0), is the maximum by size
					if (max_eow-max_sow) < (eow-sow):
						max_sow,max_eow = sow, eow

					# Shrink window by advancing start of window to exclude the first 0 in the window
					while A[sow] != 0:
						sow += 1
					sow += 1
				else:
					# Added a 0 to the current window
					M -= 1

		# End of the pass,
		# Check if the last window is bigger than the
		# maximum window seen so far
		if (max_eow-max_sow) < (eow-sow+1):
			return range(sow, eow+1)

		return range(max_sow, max_eow)


if __name__ == '__main__':
	s = Solution()
	assert s.find_max_consecutives_1s([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1], 3) == [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
	assert s.find_max_consecutives_1s([1, 1, 0, 1, 1, 0, 0, 1, 1, 1], 1) == [0,1,2,3,4]
	assert s.find_max_consecutives_1s([1, 1, 0, 1, 1, 0, 0, 1, 1, 1], 2) == [3,4,5,6,7,8,9]
	assert s.find_max_consecutives_1s([],2) == []
	assert  s.find_max_consecutives_1s([1,1,1,0,0,0,1,1,1,1,0], 3) == [0,1,2,3,4,5,6,7,8,9]

