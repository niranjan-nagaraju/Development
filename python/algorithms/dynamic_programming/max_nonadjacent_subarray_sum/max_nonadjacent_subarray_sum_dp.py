'''
https://www.hackerrank.com/challenges/max-array-sum/problem

Max Array Sum

Given an array of integers, find the subset of non-adjacent elements with the maximum sum. Calculate the sum of that subset.
For example, given an array  arr = [-2,1,3,-4,5], we have the following possible subsets:

	Subset      Sum
	[-2, 3, 5]   6
	[-2, 3]      1
	[-2, -4]    -6
	[-2, 5]      3
	[1, -4]     -3
	[1, 5]       6
	[3, 5]       8
Our maximum subset sum is 8.

Function Description
Complete the  maxSubsetSum function in the editor below.
It should return an integer representing the maximum subset sum for the given array.

maxSubsetSum has the following parameter(s):
	arr: an array of integers

Input Format
The first line contains an integer, n.
The second line contains  space-separated integers arr[i].

Output Format
Return the maximum sum described in the statement.

Sample Input 0
5
3 7 4 6 5
Sample Output 0
13
Explanation 0
Our possible subsets are
[3, 4, 5] : 12
[3, 4] : 7
[3, 6] : 9
[3. 5] : 8
[7, 6] : 13
[7, 5] : 12
[4, 5] : 9

Sample Input 1
5
2 1 5 8 4
Sample Output 1
11
Explanation 1
Possible subsets
[2, 5, 4]: 11
[2, 5] : 7
[2, 8] : 10
[2, 4] : 6
[1, 8] : 9
[1, 4] : 5
[5, 4] : 9

Sample Input 2
5
3 5 -7 8 10
Sample Output 2
15
Explanation 2
Possible subsets
[3, -7, 10] : 6
[3, -7] : -4
[3, 8] : 11
[3, 10] : 13
[5, 8] : 13
[5, 10] : 15
[-7, 10] : 3
'''


'''
Solution Outline: O(n) time
1. Maintain a DP table that stores the maximum sum until a[0..x],  (0 <= x < n)
  (candidates for the sum: a[i], a[j], a[k], .., neither of i,j,k are adjacent)
   that results in the maximum sum seen so far.
   DP table [0] : a[0]
   DP table [1] : max(a[0], a[1])
2. Given maximum-sum until a[0..j-1], maximum-sum for a[0..j] can be calculated as below -
      check maximum-sum for a[0..j-2] (we know none of the candidates for this max sum will be adjacent to each other, and 
	  since we aren't looking for a[0..j-1], wont contain a[j-1] either)
	  If maximum-sum a[0..j-2] + a[j] increases max sum,
	     check if a[j] itself is > DP[j-2]+a[j] {maximum-sum a[0..j-2]}
		   (this ensures if a[0..j-2] are all negative, then we just include a[j] as the max sum)
		   if it is, DP[j] = a[j]
		 If not, then sum is +ve, and a[j] increases max-sum
		   DP[j] = DP[j-2] + a[j]
	 else, (a[j] does not increase maximum sum)
	    copy DP[j] = maximum-sum so far == DP[j-1]
3. DP[n-1] will contain the final maximum-sum for a[0..n-1] where none of the candidates that add upto max sum are adjacent.


Sample run 1:
	a[] : [3, 5, -7, 8, 10]

	DP: [3, 5]
	max-sum = 5

	i: 2
	  DP[0] = 3
	  a[i] = -7
	  DP[0] + a[i] == -4 < max-sum
	  DP[2] = DP[1] = 5
	  DP: [3, 5, 5] 

	i: 3
	  DP[1] = 5
	  a[i] = 8
	  DP[1] + a[i] = 13 > max-sum
	    and a[i] < DP[1] + a[i] => DP[1] isnt negative
		max-sum = 13
		DP[3] = DP[1] + a[i] = 13
	  DP: [3, 5, 5, 13] 
	
	i: 4
	  DP[2] = 5
	  a[i] = 10
	  DP[2] + a[i] = 15 > max-sum, and DP[2] isnt -ve
	    DP[4] = DP[2]+a[i] = 15
		max-sum = 15
	  DP: [3, 5, 5, 13, 15] -> return 15


Sample run 2:
	a: [3, 10, 5, -8, 13]
	DP: [3, 10]
	max-sum: 10

	i: 2, a[i] = 5
	DP[0] + a[i] = 8 < max-sum
	DP[2] = max-sum = 10
	DP: [3, 10, 10]

	i: 3, a[i] = -8
	DP[1] + a[i] = 2 < max-sum
	DP[3] = max-sum = 10
	DP: [3, 10, 10, 10]

	i: 4, a[i] = 13
	DP[2] + a[i] = 10+13=23 > max-sum
	max-sum = 23
	DP[4] = max-sum = 23
	DP: [3, 10, 10, 10, 23]


Sample run 3:
	a: [3, 7, 4, 6, 5]
	DP: [3, 7]
	max-sum: 7

	i: 2, a[i] = 4
	DP[0] + a[i] = 7 > max-sum? NO
	DP[2] = max-sum = 7
	DP: [3, 7, 7]

	i: 3, a[i] = 6
	DP[1] + a[i] = 13 > max-sum
	max-sum = 13
	DP[3] = 13
	DP: [3, 7, 7, 13]

	i: 4, a[i] = 5
	DP[2] + a[i] = 12 < max-sum
	DP[4] = max-sum = 13
	DP: [3, 7, 7, 13, 13]

Sample run 4:
	a: [-2,1,3,-4,5]
	DP: [-2, 1]
	max-sum: 1

	i: 2, a[i] = 3
	DP[0] + a[i] = 1 > max-sum? NO
    a[i] > max-sum? YES
	  max-sum = a[i]
	DP[2] = 3
	DP: [-2, 1, 3]

	i: 3, a[i] = -4
	DP[1] + a[i] = -3 < max-sum
	DP[3] = 3
	DP: [-2, 1, 3, 3]

	i: 4, a[i] = 5
	DP[2] + a[i] = 8 > max-sum
	  max-sum = 8
	DP[4] = 8
	DP: [-2, 1, 3, 3, 8]
'''

def max_non_adjacent_subset_sum(arr):
	DP = [0]*len(arr)
	DP[0] = arr[0]
	DP[1] = max_sum = max(arr[0], arr[1])

	for i in xrange(2, len(arr)):
		if arr[i] > max_sum:
			max_sum = arr[i]

		if arr[i] + DP[i-2] > max_sum:
			max_sum = arr[i] + DP[i-2]

		DP[i] = max_sum

	return max_sum



if __name__ == '__main__':
	assert max_non_adjacent_subset_sum([-2, 1, 3, -4, 5]) == 8
	assert max_non_adjacent_subset_sum([3, 7, 4, 6, 5]) == 13
	assert max_non_adjacent_subset_sum([2, 1, 5, 8, 4]) == 11 
	assert max_non_adjacent_subset_sum([3, 5, -7, 8, 10]) == 15
	assert max_non_adjacent_subset_sum([3, 10, 5, -8, 13]) == 23
	assert max_non_adjacent_subset_sum([5, 5, 10, 100, 10, 5]) == 110
	assert max_non_adjacent_subset_sum([1,2,3]) == 4
	assert max_non_adjacent_subset_sum([1,20,3]) == 20

