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

# Returns the minimum number of flips, when there is a tie
# as compared to the lexicographically smaller [l,r]
class Solution:
    # @param A : string
    # @return a list of integers
    def flip(self, A):
		curr_sum =  1 if A[0] == '0' else -1
		max_sum = curr_sum

		array = [1,1]
		max_array = [1,1]
		l = 1
		for i in xrange(1, len(A)):
			x = 1 if A[i] == '0' else -1
			curr_sum = max(curr_sum+x, x)
			if curr_sum == x:
				l = (i+1)
				
			if curr_sum > max_sum:
				max_sum = curr_sum
				max_array = [l, i+1]

		if max_sum < 0:
			return []

		return max_array


if __name__ == '__main__':
	s = Solution()
	assert s.flip("0101") == [1,1]
	assert  s.flip("010") == [1,1]
	assert  s.flip("111") == []
	assert  s.flip("0100110") == [3,4]
	assert  s.flip("1101010001") == [7,9]
	assert  s.flip("0111000100010") == [5,11]

