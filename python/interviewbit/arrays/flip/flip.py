#encoding: utf-8
'''
https://www.interviewbit.com/problems/flip/

You are given a binary string(i.e. with characters 0 and 1) S consisting of characters S1, S2, …, SN. In a single operation, you can choose two indices L and R such that 1 ≤ L ≤ R ≤ N and flip the characters SL, SL+1, …, SR. By flipping, we mean change character 0 to 1 and vice-versa.

Your aim is to perform ATMOST one operation such that in final string number of 1s is maximised. If you don’t want to perform the operation, return an empty array. Else, return an array consisting of two elements denoting L and R. If there are multiple solutions, return the lexicographically smallest pair of L and R.

Notes:
Pair (a, b) is lexicographically smaller than pair (c, d) if a < c or, if a == c and b < d.
'''

'''
Count 0s as 1, 1s as -1
Max consecutive sum using kadane's
'''


class Solution:
	# @param A : string
	# @return a list of integers
	def flip(self, A):
		max_sum = 0
		curr_sum = 0
		l = 0
		max_array = None

		for i in xrange(len(A)):
			curr_sum += (1 if A[i] == '0' else -1)

			# If current sum dips below 0
			# reset to 0 (indicating no flips)
			# and set left flip index to the next index
			# so incase it is 0, we can start flipping from (i+1)
			if curr_sum < 0:
				curr_sum = 0
				l = i + 1

			if curr_sum > max_sum:
				max_sum = curr_sum
				max_array = [l+1, i+1]

		if max_array is None:
			return []

		return max_array



if __name__ == '__main__':
	s = Solution()
	assert s.flip("0101") == [1,1]
	assert  s.flip("010") == [1,1]
	assert  s.flip("111") == []
	assert  s.flip("0100110") == [1,4]
	assert  s.flip("1101010001") == [3,9]
	assert  s.flip("0111000100010") == [5,11]


