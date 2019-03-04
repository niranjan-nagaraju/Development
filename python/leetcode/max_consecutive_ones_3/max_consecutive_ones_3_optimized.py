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




##########################################################

'''
* Solution description
Consider an arbitrary sequence:
  i: 0 . . . 5 6 7 8 9 10 11 12 13 14 15 16 17 18
  A:         0 1 1 1 0 1  1  1  0  1  0  1  1  0
  k = 3
  If we are starting at 9, we can flip 3 slots containing 0s (namely 9, 13, 15)
  let 
    'start': the start of this k-slot range     [in this case, 9]
    'end':   the end of the k-slot range        [in this case, 15]

  Then A would become
    i: 0 . . . 5 6 7 8  9  10 11 12  13  14  15 16 17 18
    A:         0 1 1 1 _1_ 1  1  1  _1_  1  _1_  1  1  0

  resulting a longest 1 sequence of length 12 (for this flip of k 0s)
  This sequence has 3 parts: pre-window, window, post-window.
  The 'window' is the sequence of 1s extended/merged as a result of toggling k 0s between [start .. end]
  pre-window: is the number of 1s between 'start', and a 0 slot to the left of 'start'.
  post-window: is the number of 1s after 'end', and a 0 slot to the right of 'end'.

  In the example, start = 9, end = 15,
  window = number of 1s between 15 and 9 (including 9 and 15) == 15-9+1 == 7  
    i:  9  10 11 12  13  14  15
    A: _1_ 1  1  1  _1_  1  _1_

  pre-window = number of 1s between 9 and 5 (excluding both 9 and 5, 5 is the 0 slot to the left of 'start') = 9-5-1 = 3
    i: 5 6 7 8  9  
    A: 0 1 1 1 _1_

  post-window = number of 1s between 15 and 18 (excluding both 15 and 18, 18 is the 0 slot to the right of 'end') = 18-15-1 = 2

  Total 1s sequence as a result of flipping k(=3) 0s in this case = 
     = pre-window + window + post-window
     = 3 + 7 + 2 = 12


If we then slide over to the next k-slots, (13, 15, 18) and so on until the end
we'll have the longest 1s sequence by the end of the pass.

If 'start' is the very first 0s slot, then we use (-1) as the starting point for pre-window
 e.g.,
      i:  0  1  2  3 
      A: [1, 1, 1, 0, ....,], start = 3 
      pre-window = (3 - (-1) -1) = 3 

If 'end' is the last 0s slot, then we use (n == length(A)) as the ending point for post-window
   e.g.,
       i:           6  7  8  9         
       A: [......., 0, 1, 1, 1], 
          n = 10, end = (n-4) = 6
       post-window = (n- (n-4) -1) = (10 - 6 - 1) = 3  


Test run:
=========
   (for convenience, we filter out indices containing 0s before-hand, and store them in a list called 'zeroes')
 
     0 1 2 3 4 5 6 7 8 9 10 11 12 13
A = [0,0,0,0,1,0,1,1,1,0,1, 0, 1, 0], n = 14
k = 3
zeroes = [0,1,2,3,5,9,11,13]

Initialize: longest1s = 0

zeroes = [0,1,2,3,5,9,11,13]
+ iter 0: first k 0s in zeroes: (0,1,2)
pre-window: 0 - (-1) -1 = 0
window: 2-0+1 = 3
post-window: 3-2-1 = 0
=> 1s combined =  (0+3+0) = 3  
  i:  0 1 2 3
  A: [1,1,1,0, ...]
> longest1s = 3

zeroes = [0,1,2,3,5,9,11,13]
+ iter 1: second k 0s, : (1,2,3)
pre-window: (1-0-1) = 0
window: (3-1+1) = 3
post-window: (5-3-1) = 1 
=> 1s combined = (0 + 3 + 1)  = 4 
  i:  0 1 2 3 4 5
  A: [0,1,1,1,1,0, ...] 
> longest1s = 4

zeroes = [0,1,2,3,5,9,11,13]
+ iter 2: third set of k 0s: (2,3,5)
pre-window: (2-1-1) = 0
window:   (5-2+1) = 4
post-window: (9-5-1) = 3
=> 1s combined =  
  i:  1 2 3 4 5 6 7 8 9
  A: [0,1,1,1,1,1,1,1,0, ...]
 > longest1s = 7

zeroes = [0,1,2,3,5,9,11,13]
+ iter 3: fourth set of k 0s: (3,5,9)
pre-window: (3-2-1) = 0
window:   (9-3+1) = 7
post-window: (11-9-1) = 1
=> 1s combined = (0 + 7 + 1) = 8
  i:  2 3 4 5 6 7 8 9 10 11
  A: [0,1,1,1,1,1,1,1, 1,0, ...]
> longest1s = 8

zeroes = [0,1,2,3,5,9,11,13]
+ iter 4: fifth set of k 0s: (5,9,11)
pre-window: (5-3-1) = 1
window:  (11-5+1) = 7
post-window: (13-11-1) = 1
=> 1s combined = (11-3-1) + (13-11) = 7+2 = 9
  i:  3 4 5 6 7 8 9 10 11 12 13
  A: [0,1,1,1,1,1,1,1, 1, 1, 0]
> longest1s = 9

zeroes = [0,1,2,3,5,9,11,13]
+ iter 5: sixth set of k 0s: (9, 11, 13)
pre-window: (9-5-1) = 3
window:  (13-9+1) = 5
post-window: (14 - 13 - 1) = 0
=> 1s combined = (3 + 5 + 0) = 8
  i:  5 6 7 8 9 10 11 12 13
  A: [0,1,1,1,1, 1, 1, 1, 1]
< longest1s, (longest1s = 9) 


+ iter 6: (11, 13, ???)
  END

longest1s = 9
'''

class Solution(object):
	def longestOnes(self, A, K):
		"""
		:type A: List[int]
		:type K: int
		:rtype: int
		"""
		
		zeroes = self.filter_0s(A)
		# if k = 0, then we cannot flip any 0s in A
		# just return the current maximum 1s sequence in A
		if K == 0:
			return self.find_longest_1s(zeroes, len(A))

		longest_1s = 0
		for i in range(len(zeroes)):
			curr_longest = self.local_longest_1s(zeroes, i, K, len(A))
			if  curr_longest > longest_1s:
				longest_1s = curr_longest

			# n => some combination of flips yielded us all 1s
			# we don't have to look further as there won't be ever a sequence of 1s > n
			if curr_longest == len(A):
				break

		return longest_1s


	'''
	Given a list, zeroes,  containing positions of 0s in the original list A,
	and a 'start' position from where 'k' 0s can be flipped,
	return the longest interval containing only 1s.
	n: length of A

	e.g.
	n:  10
	0s: [3,4,5]
    k = 2
    start: 1 => (4,5) to be flipped
	=> A: [1,1,1,0,0,0,1,1,1,1]
	=> longest 1s sequence is 4
	'''
	@staticmethod
	def local_longest_1s(zeroes, startIdx, k, n):
		# There are less than k '0 slots' in the list
		# all of these can be flipped
		# effecting making everything in A to be 1s
		if k >= len(zeroes):
			#print 'k >= len(zeroes)'
			return n

		# end of window
		endIdx = startIdx + k - 1

		# we have fewer than k 0s at the end,
		# return 0 to indicate we cannot calculate without k 0s
		# NOTE: if we had < k 0s begin with, we'd have returned 'n' already
		if endIdx >= len(zeroes):
			#print 'endIdx:', endIdx
			return 0

		# boundaries of current window
		start = zeroes[startIdx]
		end = zeroes[endIdx]

		# edge of left pre-window
		if startIdx == 0:
			left_window = -1
		else:
			left_window = zeroes[startIdx-1]


		# edge of right post-window
		if end == zeroes[-1]:
			right_window = n
		else:
			right_window = zeroes[endIdx+1]


		#print 'Start: %d End: %d, leftw: %d, rightw: %d' %(start, end, left_window, right_window)

		pre_window = (start - left_window - 1)
		window = (end - start + 1)
		post_window = (right_window - end - 1)

		#print 'pre-win: %d, win: %d, post-win: %d' %(pre_window, window, post_window)

		return pre_window + window + post_window


	'''
	Given an array of 1s and 0s, filter indices which contains 0s
	and return the list of indices
	'''
	@staticmethod
	def filter_0s(array):
		zeroes = []
		for i in xrange(len(array)):
			if array[i] == 0:
				zeroes.append(i)

		return zeroes


	'''
	Calculate the longest 1s sequence in a list of 0s and 1s and return it
	e.g.
	n: 10
	0s: [3,4,5]
	=> Array: [1,1,1,0,0,0,1,1,1,1]
	=> longest 1s sequence is 4
	'''
	@staticmethod
	def find_longest_1s(zeroes, n):
		if zeroes == []:
			return n
		longest_1s = zeroes[0]
		for i in xrange(1, len(zeroes)):
			curr_seq_length = zeroes[i] - zeroes[i-1] - 1
			if curr_seq_length > longest_1s:
				longest_1s = curr_seq_length

		if (n-zeroes[-1]-1) > longest_1s:
			longest_1s = (n-zeroes[-1]-1)

		return longest_1s



if __name__ == '__main__':
	sol = Solution()
	assert(sol.filter_0s([1, 0, 1, 1, 1, 1, 1, 1, 1, 0]) == [1,9])
	assert(sol.filter_0s([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1]) == [0, 1, 4, 5, 9, 12, 13, 14])
	assert(sol.filter_0s([1,1,1,0,0,0,1,1,1,1,0]) == [3,4,5,10])


	assert(sol.find_longest_1s([3,4,5], 10) == 4)
	assert(sol.find_longest_1s([], 10) == 10)
	assert(sol.find_longest_1s([1, 9], 10) == 7)  # [1, 0, 1, 1, 1, 1, 1, 1, 1, 0]
	assert(sol.find_longest_1s([0,1,4,5,9,12,13,14], 19) == 4)  # [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1]


	assert(sol.local_longest_1s([3,4,5], 0, 2, 10) == 5) # [1,1,1,0,0,0,1,1,1,1,1]
	assert(sol.local_longest_1s([3,4,5], 1, 2, 10) == 6) # [1,1,1,0,0,0,1,1,1,1,1]
	assert(sol.local_longest_1s([3,4,5], 0, 3, 10) == 10) # [1,1,1,0,0,0,1,1,1,1,1]
	assert(sol.local_longest_1s([3,4,5], 0, 4, 10) == 10) # [1,1,1,0,0,0,1,1,1,1,1]
	assert(sol.local_longest_1s([], 0, 2, 10) == 10)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 0, 3, 14) == 3)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 1, 3, 14) == 4)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 2, 3, 14) == 7)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 3, 3, 14) == 8)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 4, 3, 14) == 9)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 5, 3, 14) == 8)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 6, 3, 14) == 0)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 7, 3, 14) == 0)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 8, 3, 14) == 0)
	assert(sol.local_longest_1s([0,1,2,3,5,9,11,13], 9, 3, 14) == 0)
	assert(sol.local_longest_1s([1, 9], 0, 1,  10) == 9)  # [1, 0, 1, 1, 1, 1, 1, 1, 1, 0]
	assert(sol.local_longest_1s([0,1,4,5,9,12,13,14],0, 4, 19) == 9)  # [0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1]

	assert sol.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 3) == 10
	assert sol.longestOnes([0,0,1,1,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1], 2) == 7
	assert sol.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 2) == 6
	assert sol.longestOnes([1,1,1,0,0,0,1,1,1,1,0], 3) == 10
	assert sol.longestOnes([0,0,0,0,1,0,1,1,1,0,1, 0, 1, 0], 3) == 9
	assert sol.longestOnes([0,0,0,0,1,0,1,1,1,0,1, 0, 1, 0], 0) == 3
	assert sol.longestOnes([0,0,1,1,1,0,0], 0) == 3
	assert sol.longestOnes([0,1,1,1,0,0,0,0], 5) == 8
