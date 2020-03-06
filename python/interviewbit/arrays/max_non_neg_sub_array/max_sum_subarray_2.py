'''
https://www.interviewbit.com/problems/max-non-negative-subarray/?ref=success-next-suggestion
'''

'''
Solution Outline:
	Keep building current subarray until a negative number is encountered
	If current sub-array has higher sum, update max sum, and mark the current sub array as max sum sub-array
'''
class Solution:
	# @param A : list of integers
	# @return a list of integers
	def maxset(self, A):
		cur_sub = []
		max_sub = []
		max_sum = 0

		for x in A:
			if x >=0:
				cur_sub.append(x)
			else: 
				# x is -ve, update max, start a new subarray
				(max_sum, max_sub) = max_sub_array(cur_sub, max_sub, max_sum)
				cur_sub = []


		# post end of the list, we might still be left with a subarray to process
		# if the array doesn't end with a -ve number
		if not cur_sub == []:
			(max_sum, max_sub) = max_sub_array(cur_sub, max_sub, max_sum)

		return max_sub
			

def sum(array):
	return reduce(lambda x,y: x+y, array, 0)

# return max subarray of sub vs maxsub and max subarray sum, 
# maxsum is current maximum sum
def max_sub_array(sub, max_sub, max_sum):
	if sum(sub) > max_sum:
		max_sum = sum(sub)
		max_sub = sub
	elif sum(sub) == max_sum:
		if len(sub) > len(max_sub):
			max_sub = sub

	return (max_sum, max_sub)


if __name__ == '__main__':
	sol = Solution()
	arr = [2, 6, -8, 1, 2, 5, -7, 2, 3]
	assert sol.maxset(arr) == [1,2,5]

	arr = [2, 7, -8, 1, 2, 5, -7, 2, 3]
	assert sol.maxset(arr) == [2,7]

	assert sol.maxset([1, 2, 5, -7, 2, 3]) == [1,2,5]

