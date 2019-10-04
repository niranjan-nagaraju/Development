'''
Longest Increasing Subsequence:
	a:[10, 22, 9, 33, 21, 50, 41, 60]
	This list has *two* longest increasing subsequences both of length 5 -
	  LIS 1: [10, 22, 33, 50, 60]
	  LIS 2: [10, 22, 33, 41, 60]
'''

'''
Solution (Dynamic Programming):
   Initially, LIS[0:n-1] = [1] -- because [x] in itself is an IS
   i: 1 -> n-1:
      LIS[i] = max(LIS[i], LIS[j]), j: 0 -> i-1
   LIS: max(LIS[])

   The sequence(atleast one) itself can be got by getting a[x], a[y],... a[z]
	  where x: rightmost index of max(LIS[]), y: rightmost index of max(LIS[])-1), ...., rightmost index of 1 
'''
class LIS(object):
	def __init__(self, numbers):
		self.numbers = numbers
		self.lis_table = None
		self.lis_sequence = None
		self.lis_len = 0



	'''
	Create a DP table of LIS for i:0 -> n-1, using dynamic programming
	'''
	def make_lis_table(self):
		# LIS[i]: Will contain longest increasing subsequence length ending at index i in the numbers list
		# Each number is an LIS by itself, so initialize each index to 1
		lis_table = [1] * len(self.numbers)
		for i in range(1, len(self.numbers)):
			for j in range(0, i):
				if self.numbers[i] > self.numbers[j]:
					lis_table[i] = max(lis_table[i], lis_table[j]+1)

		self.lis_table = lis_table
		self.lis_len = max(lis_table)
		return lis_table
		


	'''
	Get length of the LIS from given numbers
	'''
	def lis_length(self):
		# First time calling this function,
		# Create the DP table first
		if self.lis_table is None:
			self.make_lis_table()

		return self.lis_len



	'''
	Get subsequence making the LIS from given numbers
	NOTE: There can be many longest increasing subsequences
	 This function reconstructs and returns one of them from the DP table
	'''
	def get_lis_sequence(self):
		# find first occurence of 'x' from right of arr, arr[0:rightWin]
		def rfind(arr, x, rightWin):
			for i in range(rightWin, -1, -1):
				if arr[i] == x:
					return i
			return -1

		# Previously computed LIS is still valid,
		# return the list of numbers making the LIS
		if self.lis_sequence is not None:
			return self.lis_sequence

		# First time calling this function,
		# Create the DP table first
		lis_table = self.lis_table
		if lis_table is None:
			lis_table = self.make_lis_table()

		lis_sequence = []
		curr_lis_len = self.lis_len
		idx = len(lis_table)
		for i in range(self.lis_len):
			idx = rfind(lis_table, curr_lis_len, idx-1)
			lis_sequence.insert(0, self.numbers[idx])
			curr_lis_len -= 1

		self.lis_sequence = lis_sequence
		return lis_sequence



if __name__ == "__main__":
	l1 = LIS([10,22,9,33,21,50,41,60])
	l2 = LIS([5,4,3,2,1])
	l3 = LIS([1,2,3,4,5,4,3,2,1])
	l4 = LIS([10, 9, 2, 5, 3, 7, 101, 18])

	assert l1.make_lis_table() == [1, 2, 1, 3, 2, 4, 4, 5]
	assert l3.make_lis_table() == [1, 2, 3, 4, 5, 4, 3, 2, 1]

	assert l1.lis_length() == 5
	assert l1.lis_table == [1, 2, 1, 3, 2, 4, 4, 5]
	assert l1.get_lis_sequence() == [10, 22, 33, 41, 60]

	assert l2.lis_length() == 1
	assert l2.get_lis_sequence() == [1]
	assert l2.lis_table == [1, 1, 1, 1, 1]

	assert l3.lis_length() == 5
	assert l3.lis_table == [1, 2, 3, 4, 5, 4, 3, 2, 1]
	assert l3.get_lis_sequence() == [1, 2, 3, 4, 5]

	assert l4.lis_length() == 4
	assert l4.get_lis_sequence() == [2,3,7,18]
	assert l4.lis_table == [1, 1, 1, 2, 2, 3, 4, 4]

