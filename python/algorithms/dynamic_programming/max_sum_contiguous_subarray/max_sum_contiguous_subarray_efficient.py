'''
https://www.interviewbit.com/problems/max-sum-contiguous-subarray/

Max Sum Contiguous Subarray

Find the contiguous subarray within an array, A of length N which has the largest sum.

Input Format:

The first and the only argument contains an integer array, A.
Output Format:

Return an integer representing the maximum possible sum of the contiguous subarray.
Constraints:

1 <= N <= 1e6
-1000 <= A[i] <= 1000
For example:

Input 1:
    A = [1, 2, 3, 4, -10]

Output 1:
    10

Explanation 1:
    The subarray [1, 2, 3, 4] has the maximum possible sum of 10.

Input 2:
    A = [-2, 1, -3, 4, -1, 2, 1, -5, 4]

Output 2:
    6

Explanation 2:
    The subarray [4,-1,2,1] has the maximum possible sum of 6.
'''

'''
Solution Outline: (Kadane's)
	1. Initialize curr_sum = A[0], max_sum = A[0]
	2. Scan A[1..n-1], at any index, x, 
	   if A[x] increases curr_sum, add A[x] to curr_sum
	     except when A[x] on its own > curr_sum, then set curr_sum to A[x]
	 if curr_sum > max_sum => set max_sum to cur_sum

Sample run 1:
	A : [1, 2, 3, -10, 20]
	curr_sum: 1, max_sum: 1

	i: 1, a[i] = 2
	curr_sum + 2 == 3
	curr_sum = max(curr_sum+a[i], a[i]) == max(3, 2) = 3
	curr_sum > max_sum
	max_sum = 3

	i: 2, a[i] == 3
	curr_sum + a[i] = 6
	curr_sum = max(curr_sum+a[i], a[i]) == max(6, 3) = 6
	curr_sum > max_sum => max_sum = 6

	i: 3, a[i] == -10
	curr_sum + a[i] = -4
	curr_sum = max(curr_sum+a[i], a[i]) == max(-4, -10) = -4
	
	i: 4, a[i] == 20
	curr_sum + a[i] == 16
	curr_sum = max(curr_sum+a[i], a[i]) == max(16, 20) == 20
	max_sum = 20


Sample run 2:
	A: [-4, -2, -3, 1, -2]
	curr_sum = max_sum = -4

	i: 1, a[i] = -2
	a[i] + curr_sum = -6
	curr_sum = max(curr_sum+a[i], a[i]) == max(-6, -2) == -2
	> max_sum => -2

	i: 2, a[i] = -3
	a[i] + curr_sum = -5
	curr_sum = max(a[i]+curr_sum, a[i]) == max(-5, -3) == -3

	i: 3, a[i] = 1
	a[i] + curr_sum = -2
	curr_sum = max(a[i] + curr_sum, a[i]) == max(-2, 1) == 1
	> max_sum => 1

	i: 4, a[i] = -2
	a[i] + curr_sum = -1
	curr_sum = max(a[i] + curr_sum, a[i]) == max(-1, -2) == -1

	max_sum = 1


Sample run 3:
    A = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
	curr_sum = max_sum = -2

	i: 1, a[i] = 1
	curr_sum + a[i] == -1
	curr_sum = max(curr_sum + a[i], a[i]) == max(-1, 1) == 1
	> max_sum => 1
	
	i: 2, a[i] = -3
	a[i] + curr_sum == -2
	curr_sum = max(curr_sum + a[i], a[i]) == max(-2, -3) = -2

	i: 3, a[i] = 4
	a[i] + curr_sum == 2
	curr_sum = max(curr_sum + a[i], a[i]) == max(2, 4) = 4
	> max_sum = 4

	i: 4, a[i] = -1
	a[i] + curr_sum == 3
	curr_sum = max(curr_sum + a[i], a[i]) == max(3, -1) = 3

	i: 5, a[i] = 2
	a[i] + curr_sum == 5
	curr_sum = max(curr_sum + a[i], a[i]) == max(5, 2) = 5
	> max_sum = 5

	i: 6, a[i] = 1
	a[i] + curr_sum == 6
	curr_sum = max(curr_sum + a[i], a[i]) == max(6, 1) = 6
	> max_sum = 6
	 
	i: 7, a[i] = -5
	a[i] + curr_sum == 1
	curr_sum = max(curr_sum + a[i], a[i]) == max(1, -5) = 1

	i: 8, a[i] = 4
	a[i] + curr_sum == 5
	curr_sum = max(curr_sum + a[i], a[i]) == max(5, 4) = 5

	max_sum = 6
'''
class MaxContiguousSumSubarray:
	# return the maximum sum of any contiguous subarrays of the given array
	def sum(self, a):
		max_sum = curr_sum = a[0]
		for i in xrange(1, len(a)):
			curr_sum = max(curr_sum+a[i], a[i])
			if curr_sum > max_sum:
				max_sum = curr_sum

		return max_sum


	# return the maximum sum of any contiguous subarrays of the given array
	def subarray(self, a):
		max_sum = curr_sum = a[0]
		curr_subarray = [0, 0]
		max_subarray = [0, 0]
		for i in xrange(1, len(a)):
			curr_sum = max(curr_sum+a[i], a[i])

			# a[i] was max
			if curr_sum == a[i]:
				curr_subarray[0] = curr_subarray[1] = i
			else:
				# a[i] was added to curr_sum
				curr_subarray[1] = i
				
			if curr_sum > max_sum:
				max_sum = curr_sum
				max_subarray = curr_subarray[:]

		return a[max_subarray[0]:max_subarray[1]+1]


if __name__ == '__main__':
	s = MaxContiguousSumSubarray()
	assert s.sum([-2, -3, 4, -1, -2, 1, 5, -3]) == 7
	assert s.subarray([-2, -3, 4, -1, -2, 1, 5, -3]) == [4, -1, -2, 1, 5]

	assert s.sum([-2, 1, -3, 4, -1, 2, 1, -5, 4]) == 6
	assert s.subarray([-2, 1, -3, 4, -1, 2, 1, -5, 4]) == [4, -1, 2, 1]

	assert s.sum([-4, -2, -3, 1, -2]) == 1
	assert s.subarray([-4, -2, -3, 1, -2]) == [1]

	assert s.sum([1, 2, 3, -10, 20]) == 20
	assert s.subarray([1, 2, 3, -10, 20]) == [20]

	assert s.sum([1, 2, 3, 4, 5]) == 15
	assert s.subarray([1, 2, 3, 4, 5]) == [1,2,3,4,5]

	assert s.sum([2,1,5,7,8]) == 23
	assert s.subarray([2,1,5,7,8]) == [2,1,5,7,8]

