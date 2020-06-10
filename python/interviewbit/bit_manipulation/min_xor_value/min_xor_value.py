#encoding: utf-8
'''
https://www.interviewbit.com/problems/min-xor-value/

Min XOR value

Given an integer array A of N integers, find the pair of integers in the array which have minimum XOR value. Report the minimum XOR value.

Input Format:
    First and only argument of input contains an integer array A

Output Format:
    return a single integer denoting minimum xor value

Constraints:
2 <= N <= 100 000  
0 <= A[i] <= 1 000 000 000

For Examples :
Example Input 1:
    A = [0, 2, 5, 7]
Example Output 1:
    2
Explanation:
    0 xor 2 = 2
Example Input 2:
    A = [0, 4, 7, 9]
Example Output 2:
    3
'''

'''
Solution Outline: (Brute-force, O(n²) time)
  Calculate xor of each unique pair, return the smallest xor of any pair
'''
class Solution:
	def find_min_xor_pair(self, A):
		min_xor_value = A[0]^A[1]
		for i in xrange(1, len(A)):
			for j in xrange(i+1, len(A)):
				if min_xor_value > (A[i]^A[j]):
					min_xor_value = A[i]^A[j]
		return min_xor_value



if __name__ == '__main__':
	s = Solution()
	assert s.find_min_xor_pair([0,2,5,7]) == 2
	assert s.find_min_xor_pair([0,4,7,9]) == 3
	assert s.find_min_xor_pair([5,10,25,20,15,26]) == 3
	assert s.find_min_xor_pair([5,10,25,20,15]) == 5
	assert s.find_min_xor_pair([ 3, 2, 13, 1, 5, 13, 0, 13, 13 ]) == 0


