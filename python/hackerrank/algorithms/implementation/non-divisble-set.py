'''

https://www.hackerrank.com/challenges/non-divisible-subset

Given a set, S, of n distinct integers, print the size of a maximal subset, S', of where the sum of any numbers in S' are not evenly divisible by ~K.

Input Format

The first line contains space-separated integers, n and k, respectively.
The second line contains n space-separated integers (we'll refer to the ith value as ai) describing the unique values of the set.

Constraints
	1 <= n <=10**5
	1 <= k <= 100
	1 <= ai <= 10**9
    All of the given numbers are distinct.

Output Format
	Print the size of the largest possible subset ().

Sample Input
	4 3
	1 7 2 4

Sample Output
	3

Explanation
	The largest possible subset of integers is {1, 7, 4}, because no two integers will have a sum that is evenly divisible by k=3:
'''


# compare pairs (nC2) and return subset whose pairs aren't divisible by k
# e.g. 1 7 2 4
#	1,7 1,2, 1,4
#	7,2 7,4
#	2,4
def compare_pairs_and_get_subset(l, n , k):
	non_divisible_subset = set()
	for i in xrange(n-1):
		for j in xrange(i+1, n):
			if (l[i] + l[j]) % k != 0:
				non_divisible_subset.add(l[i])
				non_divisible_subset.add(l[j])
				print 'Adding ', (l[i], l[j])

	return len(non_divisible_subset)


n, k = map(int, raw_input().split())
l = map(int, raw_input().split())
print compare_pairs_and_get_subset(l, n, k)
