'''
https://leetcode.com/problems/max-consecutive-ones-iii/

Given an array A of 0s and 1s, we may change up to K values from 0 to 1.

Return the length of the longest (contiguous) subarray that contains only 1s. 

 Example 1:
 Input: A = [1,1,1,0,0,0,1,1,1,1,0], K = 2
 Output: 6
 Explanation: 
 [1,1,1,0,0,1,1,1,1,1,1]
 Bolded numbers were flipped from 0 to 1.  The longest subarray is underlined.

 Example 2:
 Input: A = [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], K = 3
 Output: 10
 Explanation: 
 [0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1]
 Bolded numbers were flipped from 0 to 1.  The longest subarray is underlined.
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
	def longestOnes(self, A, M):
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
			return (eow-sow+1)

		return (max_eow-max_sow)


if __name__ == '__main__':
	s = Solution()
	assert s.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1], 3) == len([2, 3, 4, 5, 6, 7, 8, 9, 10, 11])
	assert s.longestOnes([1, 1, 0, 1, 1, 0, 0, 1, 1, 1], 1) == len([0,1,2,3,4])
	assert s.longestOnes([1, 1, 0, 1, 1, 0, 0, 1, 1, 1], 2) == len([3,4,5,6,7,8,9])
	assert s.longestOnes([],2) == 0
	assert s.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 3) == 10

	assert s.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 3) == 10
	assert s.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 2) == 7
	assert s.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 2) == 6
	assert s.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 3) == 10
	assert s.longestOnes([0,0,0,0,1,0,1,1,1,0,1, 0, 1, 0], 3) == 9
	assert s.longestOnes([0,0,0,0,1,0,1,1,1,0,1, 0, 1, 0], 0) == 3
	assert s.longestOnes([0,0,1,1,1,0,0], 0) == 3
	assert s.longestOnes([0,1,1,1,0,0,0,0], 5) == 8

