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
class LIS(object):
	def __init__(self, array):
		self.array = array

	def calculate_lis_r(self, n=None):
		def lis_util(n):
			curr_inc_seq_len = 1
			curr_inc_seq = [array[n]]
			for i in range(n):
				seq_i = lis_util(i)
				if array[n] > array[i] and len(seq_i) + 1 > curr_inc_seq_len:
					curr_inc_seq_len = len(seq_i) + 1
					curr_inc_seq = seq_i + [array[n]]

					if curr_inc_seq_len > lis_sequence_and_len[1]:
						lis_sequence_and_len[1] = curr_inc_seq_len
						lis_sequence_and_len[0] = curr_inc_seq

			return curr_inc_seq

		array = self.array
		if not array:
			return ([], 0)
	
		# By default, calculate LIS for the whole array
		if n is None or n > len(array):
			n = len(array)

		lis_sequence_and_len = [None, 0]
		lis_util(n-1)

		# We couldn't find an increasing sequence
		# Just set [array[0]] as LIS
		if lis_sequence_and_len[0] is None:
			lis_sequence_and_len = [array[0]], 1

		return lis_sequence_and_len[0], lis_sequence_and_len[1]


	'''
	Get length of the LIS of the subarray, numbers[0:n] (or first n numbers)
	0 < n <= len(numbers)
	if n is not specified explicitly, calculate LIS for the whole array
	'''
	def get_lis_length(self, n=None):
		return self.calculate_lis_r(n)[1]


	'''
	Get subsequence making the LIS of first n numbers (subarray [0:n]) from given numbers
	0 < n <= len(numbers)
	if n is not specified explicitly, calculate LIS for the whole array
	NOTE: There can be many longest increasing subsequences
	 This function returns one of them
	'''
	def get_lis_sequence(self, n=None):
		return self.calculate_lis_r(n)[0]



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
	# Subarray:  [10, 22, 9, 33, 21, 50]
	assert l1.get_lis_length(6) == 4 
	assert l1.get_lis_sequence(6) == [10, 22, 33, 50]

	l2 = LIS([5,4,3,2,1])
	assert l2.get_lis_length() == 1
	assert l2.get_lis_sequence() == [5]

	l3 = LIS([1,2,3,4,5,4,3,2,1])
	assert l3.get_lis_length() == 5
	assert l3.get_lis_sequence() == [1, 2, 3, 4, 5]
	for x in xrange(1, 6):
		assert l3.get_lis_length(x) == x
		assert l3.get_lis_sequence(x) == range(1, x+1)
	assert l3.get_lis_length(100) == 5   # n > len(numbers) => whole array
	assert l3.get_lis_sequence(100) == [1, 2, 3, 4, 5] # n > len(numbers) => whole array

	l4 = LIS([10, 9, 2, 5, 3, 7, 101, 18])
	# Find LIS length for first four numbers [10, 9, 2, 5]
	assert l4.get_lis_length(4) == 2
	assert l4.get_lis_sequence(4) == [2,5]
	assert l4.get_lis_length() == 4
	assert l4.get_lis_sequence() == [2,5,7,18]


