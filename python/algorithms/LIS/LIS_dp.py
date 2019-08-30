'''
Longest Increasing Subsequence:
	a:[10, 22, 9, 33, 21, 50, 41, 60]
	This list has *two* longest increasing subsequences both of length 5 -
	  LIS 1: [10, 22, 33, 50, 60]
	  LIS 2: [10, 22, 33, 41, 60]

Solution:
   Initially, LIS[0:n-1] = [1] -- because [x] in itself is an IS
   i: 1 -> n-1:
      LIS[i] = max(LIS[i], LIS[j]), j: 0 -> i-1
   LIS: max(LIS[])

   The sequence(atleast one) itself can be got by getting a[x], a[y],... a[z]
	  where x: rightmost index of max(LIS[]), y: rightmost index of max(LIS[])-1), ...., rightmost index of 1 
'''

# find first occurence of 'x' from right of arr, arr[0:rightWin]
def rfind(arr, x, rightWin):
	for i in range(rightWin, -1, -1):
		if arr[i] == x:
			return i

	return -1


# Create a table of LIS for i:0 -> n-1, using dynamic programming
def make_lis_table(numbers):
	LIS_table = [1] * len(numbers)
	for i in range(1, len(numbers)):
		for j in range(0, i):
			if numbers[i] > numbers[j]:
				LIS_table[i] = max(LIS_table[i], LIS_table[j]+1)
	return LIS_table
	
def longest_increasing_subsequence(numbers):
	LIS_table = make_lis_table(numbers)
	lis_len = max(LIS_table)

	LIS = []
	curr_lis_len = lis_len
	idx = len(LIS_table)
	for i in range(lis_len):
		idx = rfind(LIS_table, curr_lis_len, idx-1)
		LIS.insert(0, numbers[idx])
		curr_lis_len -= 1

	return lis_len, LIS



if __name__ == "__main__":
	assert make_lis_table([10,22,9,33,21,50,41,60]) == [1, 2, 1, 3, 2, 4, 4, 5]
	assert make_lis_table([5,4,3,2,1]) == [1, 1, 1, 1, 1]
	assert make_lis_table([1,2,3,4,5,4,3,2,1]) == [1, 2, 3, 4, 5, 4, 3, 2, 1]
	assert make_lis_table([10, 9, 2, 5, 3, 7, 101, 18]) == [1, 1, 1, 2, 2, 3, 4, 4]

	assert longest_increasing_subsequence([10,22,9,33,21,50,41,60]) == (5, [10, 22, 33, 41, 60])
	assert longest_increasing_subsequence([5,4,3,2,1]) == (1, [1])
	assert longest_increasing_subsequence([1,2,3,4,5,4,3,2,1]) == (5, [1, 2, 3, 4, 5])
	assert longest_increasing_subsequence([10, 9, 2, 5, 3, 7, 101, 18]) == (4, [2,3,7,18])

