'''
Longest Increasing Subsequence:
	a:[10, 22, 9, 33, 21, 50, 41, 60]
	This list has *two* longest increasing subsequences both of length 5 -
	  LIS 1: [10, 22, 33, 50, 60]
	  LIS 2: [10, 22, 33, 41, 60]
'''


'''
Naive Solution: Uses the recurrence formula (O2^n)
  Let lis(n) be length of a longest increasing sequence ending at array[n]
  Then lis(n) = max {1,  lis(i) + 1 for i in [0..n-1] if array[n] > array[i]}
  e.g., array = [1,4,3]
  lis(0) = 1
  lis(1) = max {1, lis(0)+1 because 4 > 1} == max{1, 2} = 2
  lis(2) = max {1, lis(0)+1 because 3 > 1, lis(1) is not relevant as 3 < 4} = max{1, 2}
'''

from collections import defaultdict
class LIS(object):
	def __init__(self, array):
		self.array = array
		self.memoized_cache = defaultdict(lambda: None)
		self.lis_sequence = None
		self.lis_sequence_len = 0

	def calculate_lis_r(self):
		def lis_util(n):
			if self.memoized_cache[n] is not None:
				return self.memoized_cache[n]

			curr_inc_seq_len = 1
			curr_inc_seq = [array[n]]
			for i in range(n):
				seq_i = lis_util(i)
				if array[n] > array[i] and len(seq_i) + 1 > curr_inc_seq_len:
					curr_inc_seq_len = len(seq_i) + 1
					curr_inc_seq = seq_i + [array[n]]

					if curr_inc_seq_len > self.lis_sequence_len:
						self.lis_sequence_len = curr_inc_seq_len
						self.lis_sequence = curr_inc_seq

			self.memoized_cache[n] = curr_inc_seq
			return curr_inc_seq

		array = self.array
		if not array:
			return ([], 0)
		
		lis_util(len(array)-1)
		# We couldn't find an increasing sequence
		# Just set [array[0]] as LIS
		if self.lis_sequence is None:
			self.lis_sequence = [array[0]]
			self.lis_sequence_len = 1
			self.memoized_cache[len(array)-1] = self.lis_sequence

		return self.lis_sequence, self.lis_sequence_len


	def get_lis_length(self):
		if not self.lis_sequence:
			self.calculate_lis_r()

		return self.lis_sequence_len


	def get_lis_sequence(self):
		if not self.lis_sequence:
			self.calculate_lis_r()

		return self.lis_sequence



if __name__ == '__main__':
	assert LIS([]).calculate_lis_r() == ([], 0)

	array = [1, 5, 2, 3, 4, 7, 2]
	l = LIS(array)
	assert l.calculate_lis_r() == ([1, 2, 3, 4, 7], 5)
	assert l.get_lis_sequence() == [1, 2, 3, 4, 7]
	assert l.get_lis_length() == 5

	l1 = LIS([10,22,9,33,21,50,41,60])
	assert l1.get_lis_length() == 5
	assert l1.get_lis_sequence() == [10, 22, 33, 50, 60]

	l2 = LIS([5,4,3,2,1])
	assert l2.get_lis_length() == 1
	assert l2.get_lis_sequence() == [5]

	l3 = LIS([1,2,3,4,5,4,3,2,1])
	assert l3.get_lis_length() == 5
	assert l3.get_lis_sequence() == [1, 2, 3, 4, 5]

	l4 = LIS([10, 9, 2, 5, 3, 7, 101, 18])
	assert l4.get_lis_length() == 4
	assert l4.get_lis_sequence() == [2,5,7,18]


