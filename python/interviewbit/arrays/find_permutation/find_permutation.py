'''
https://www.interviewbit.com/problems/find-permutation/

Find Permutation
Given a positive integer n and a string s consisting only of letters D or I, you have to find any permutation of first n positive integer that satisfy the given input string.

D means the next number is smaller, while I means the next number is greater.

Notes

Length of given string s will always equal to n - 1
Your solution should run in linear time and space.

Example :

Input 1:
n = 3
s = ID
Return: [1, 3, 2]
'''


'''
Solution Outline:
	1. Generate an increasing sequence, seq [1..n]
	2. Since the sequence is already increasing, the Is dont need any change.
	3. For each contiguous sequence of Ds {i..j} reverse seq[i..j]
	   seq[j+1] is automatically increasing because seq[i]<seq[i+1]<..seq[j]<seq[j+1]
'''
class Solution:
	# reverse a[i..j]
	def reverse(self, a, i, j):
		while i < j:
			a[i], a[j] = a[j], a[i]
			i += 1
			j -= 1

	def find_permutation(self, n, s):
		seq = range(1, n+1)
		i = 0
		while i < n-1:  # s length is n-1
			curr_dec_seq_start = None
			while i < n-1 and s[i] == 'D':
				if curr_dec_seq_start == None:
					curr_dec_seq_start = i
				i += 1

			if curr_dec_seq_start is not None:
				# End of 'D' sequence
				# i is at the beginning of the next 'I' sequence
				# reverse i=k+1 elements starting from the index where 'D' starts
				# k: length of the D sequence
				# k=1 => reverse two elements, and so on.
				self.reverse(seq, curr_dec_seq_start, i)
			else:
				# We are in an 'I' sequence
				i += 1 # skip ahead this 'I'

		return seq



if __name__ == '__main__':
	s = Solution()
	assert s.find_permutation(2, 'I') == [1,2]
	assert s.find_permutation(2, 'D') == [2,1]
	assert s.find_permutation(3, 'ID') == [1,3,2]
	assert s.find_permutation(3, 'DI') == [2,1,3]
	assert s.find_permutation(6, 'IDDID') == [1,4,3,2,6,5]
	assert s.find_permutation(6, 'DDDDD') == [6,5,4,3,2,1]
	assert s.find_permutation(6, 'DIDID') == [2,1,4,3,6,5]
	assert s.find_permutation(6, 'IDIDI') == [1,3,2,5,4,6]

