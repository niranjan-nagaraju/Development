'''
https://www.interviewbit.com/problems/count-and-say/

Count And Say

The count-and-say sequence is the sequence of integers beginning as follows:

1, 11, 21, 1211, 111221, ...
1 is read off as one 1 or 11.
11 is read off as two 1s or 21.

21 is read off as one 2, then one 1 or 1211.

Given an integer n, generate the nth sequence.

Note: The sequence of integers will be represented as a string.

Example:
if n = 2,
the sequence is 11.
'''

class Solution:
	# @param n : integer
	# @return a string
	def count_and_say(self, n):
		# Generate next sequence from current one
		# [1,1,2,1] -> [2,1,1,2,1,1]
		def next_seq(curr):
			ns = []
			i = 0
			while i < len(curr):
				c = curr[i]
				count_c = 0
				while i<len(curr) and c == curr[i]:
					i += 1
					count_c += 1
				ns += [count_c, c]

			return ns

		start = [1]
		i = 1
		while i < n:
			start = next_seq(start)
			i += 1

		return reduce(lambda acc,x: acc+`x`, start, '')


if __name__ == '__main__':
	s = Solution()
	assert s.count_and_say(1) == '1'
	assert s.count_and_say(2) == '11'
	assert s.count_and_say(3) == '21'
	assert s.count_and_say(4) == '1211'
	assert s.count_and_say(5) == '111221'
	assert s.count_and_say(6) == '312211'
	assert s.count_and_say(7) == '13112221'

