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
	Sample run:
	A: [10, 22, 9, 33, 21, 50, 41, 60]
	i:  0   1   2  3   4   5   6   7 

	LIS: [1, 1, 1, 1, 1, 1, 1, 1]

	i: 1
	  j: 0, 22 > 10 => LIS[1] = max(LIS[0]+1, LIS[1]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 1, 1, 1, 1, 1]

	i: 2
	  j: 0, 9 > 10? NO 
	  j: 1, 9 > 22? NO
	  LIS: [1, 2, 1, 1, 1, 1, 1, 1]

	i: 3
	  j: 0, 33 > 10 => LIS[3] = max(LIS[0]+1, LIS[3]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 2, 1, 1, 1, 1]
	  j: 1, 33 > 22 => LIS[3] = max(LIS[1]+1, LIS[3]) == max(3, 2) == 3
	  LIS: [1, 2, 1, 3, 1, 1, 1, 1]
	  j: 2, 33 > 9 => LIS[3] = max(LIS[2]+1, LIS[3]) == max(2, 3) == 3

	i: 4
	  j: 0, 21 > 10 => LIS[4] = max(LIS[0]+1, LIS[4]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 3, 2, 1, 1, 1]
	  j: 1, 21 > 22? NO
	  j: 2, 21 > 9 => LIS[4] = max(LIS[2]+1, LIS[4]) == max(2, 2) == 2
	  j: 3, 21 > 33? NO

	i: 5
	  j: 0, 50 > 10 => LIS[5] = max(LIS[0]+1, LIS[5]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 3, 2, 2, 1, 1]
	  j: 1, 50 > 22 => LIS[5] = max(LIS[1]+1, LIS[5]) == max(3, 2) == 3
	  LIS: [1, 2, 1, 3, 2, 3, 1, 1]
	  j: 2, 50 > 9 => LIS[5] = max(LIS[2]+1, LIS[5]) == max(2, 3) == 3
	  LIS: [1, 2, 1, 3, 2, 3, 1, 1]
	  j: 3, 50 > 33 => LIS[5] = max(LIS[3]+1, LIS[5]) == max(4, 3) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 1, 1]
	  j: 4, 50 > 21 => LIS[5] = max(LIS[4]+1, LIS[5]) == max(3, 4) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 1, 1]

	i: 6
	  j: 0, 41 > 10 => LIS[6] = max(LIS[0]+1, LIS[6]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 3, 2, 4, 2, 1]
	  j: 1, 41 > 22 => LIS[6] = max(LIS[1]+1, LIS[6]) == max(3, 2) == 3
	  LIS: [1, 2, 1, 3, 2, 4, 3, 1]
	  j: 2, 41 > 9 => LIS[6] = max(LIS[2]+1, LIS[6]) == max(2, 3) == 3
	  LIS: [1, 2, 1, 3, 2, 4, 3, 1]
	  j: 3, 41 > 33 => LIS[6] = max(LIS[3]+1, LIS[6]) == max(4, 3) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 4, 1]
	  j: 4, 41 > 21 => LIS[6] = max(LIS[4]+1, LIS[6]) == max(3, 4) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 4, 1]
	  j: 5, 41 > 50? NO
	  LIS: [1, 2, 1, 3, 2, 4, 4, 1]

	i: 7
	  j: 0, 60 > 10 => LIS[7] = max(LIS[0]+1, LIS[7]) == max(2, 1) == 2
	  LIS: [1, 2, 1, 3, 2, 4, 4, 2]
	  j: 1, 60 > 22 => LIS[7] = max(LIS[1]+1, LIS[7]) == max(3, 1) == 3
	  LIS: [1, 2, 1, 3, 2, 4, 4, 3]
	  j: 2, 60 > 9 => LIS[7] = max(LIS[2]+1, LIS[7]) == max(2, 3) == 3
	  LIS: [1, 2, 1, 3, 2, 4, 4, 3]
	  j: 3, 60 > 33 => LIS[7] = max(LIS[3]+1, LIS[7]) == max(4, 3) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 4, 4]
	  j: 4, 60 > 21 => LIS[7] = max(LIS[4]+1, LIS[7]) == max(3, 4) == 4
	  LIS: [1, 2, 1, 3, 2, 4, 4, 4]
	  j: 5, 60 > 50 => LIS[7] = max(LIS[5]+1, LIS[7]) == max(5, 4) == 5
	  LIS: [1, 2, 1, 3, 2, 4, 4, 5]
	  j: 6, 60 > 41 => LIS[7] = max(LIS[6]+1, LIS[7]) == max(5, 4) == 5
	  LIS: [1, 2, 1, 3, 2, 4, 4, 5]

	  LIS length: max(LIS) = 5
	  LIS sequence: {10,22,33,41,60}
	'''
	def make_lis_table(self):
		if not self.numbers:
			return []

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
	def get_lis_length(self):
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
	assert LIS([]).get_lis_length() == 0
	assert LIS([]).get_lis_sequence() == []

	l1 = LIS([10,22,9,33,21,50,41,60])
	assert l1.make_lis_table() == [1, 2, 1, 3, 2, 4, 4, 5]
	assert l1.get_lis_length() == 5
	assert l1.lis_table == [1, 2, 1, 3, 2, 4, 4, 5]
	assert l1.get_lis_sequence() == [10, 22, 33, 41, 60]

	l2 = LIS([5,4,3,2,1])
	assert l2.get_lis_length() == 1
	assert l2.get_lis_sequence() == [1]
	assert l2.lis_table == [1, 1, 1, 1, 1]

	l3 = LIS([1,2,3,4,5,4,3,2,1])
	assert l3.make_lis_table() == [1, 2, 3, 4, 5, 4, 3, 2, 1]
	assert l3.get_lis_length() == 5
	assert l3.lis_table == [1, 2, 3, 4, 5, 4, 3, 2, 1]
	assert l3.get_lis_sequence() == [1, 2, 3, 4, 5]

	l4 = LIS([10, 9, 2, 5, 3, 7, 101, 18])
	assert l4.get_lis_length() == 4
	assert l4.get_lis_sequence() == [2,3,7,18]
	assert l4.lis_table == [1, 1, 1, 2, 2, 3, 4, 4]

	assert LIS([1, 5, 2, 3, 4, 7, 2]).get_lis_sequence() == [1, 2, 3, 4, 7]
	assert LIS([1, 5, 2, 3, 4, 7, 2]).make_lis_table() == [1, 2, 2, 3, 4, 5, 2]

