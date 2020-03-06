'''
https://www.interviewbit.com/problems/max-non-negative-subarray/?ref=success-next-suggestion
'''

'''
Solution Outline:
	Create sub-arrays separated by -ve numbers
	Return one with the maximum sum.
'''
class Solution:
	# @param A : list of integers
	# @return a list of integers
	def maxset(self, A):
		sub_arrays = create_sub_arrays(A)
		return max_sub_array(sub_arrays)

def create_sub_arrays(array):
	sub_arrays = []
	sub = []
	for x in array:
		if x >= 0:
			sub.append(x)
		else: # -ve number, flush previous sub array
			if not sub == []:
				sub_arrays.append(sub)
			sub = []

	if not sub == []:
		sub_arrays.append(sub)

	return sub_arrays


def sum(array):
	return reduce(lambda x,y: x+y, array, 0)

def max_sub_array(sub_arrays):
	max_sum = 0
	max_sub_array = []
	for sub in sub_arrays:
		if sum(sub) > max_sum:
			max_sum = sum(sub)
			max_sub_array = sub
		elif sum(sub) == max_sum:
			if len(sub) > len(max_sub_array):
				max_sub_array = sub


	return max_sub_array


if __name__ == '__main__':
	sol = Solution()
	arr = [1, 2, 5, -7, 2, 3]
	assert sol.maxset(arr) == [1,2,5]

	arr = [2, 6, -8, 1, 2, 5, -7, 2, 3]
	assert sol.maxset(arr) == [1,2,5]

	arr = [2, 7, -8, 1, 2, 5, -7, 2, 3]
	assert sol.maxset(arr) == [2,7]


