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




def get_max_non_divisible_subset(l, n, k):
	'''
	(a+b) % k == a%k + b%k
	k | (a+b) if (a%k) + (b%k) == k OR 0 (0, when k|a and k|b)
	'''
	# 'k' buckets, each will be filled based on how many numbers exist with that modulo
	# mods[i] will be counted for every ai % k == i
	mods = [0]*k

	for x in l:
		mods[x % k] += 1
	

	'''
	Elements from mods[i] and mods[k-i] cant both be there in the same subset as their sum will be divisible by k
	Pick max(mods[i], mods[k-i]) to be in the subset 

	mods[0] are all divisible, ONLY one of its elements can be included in the non-divisible-subset
	'''

	nd_subset_count = 1 if mods[0] > 0 else 0 # or just min(mods[0], 1)

	i = 1 
	j = k-i

	while i < j:
		nd_subset_count += max(mods[i], mods[j])
		i += 1
		j -= 1

	'''
	if k is even, then it'll have odd number of remainders
	e.g. 
	6: (1+5) (2+4) (3+3) => Only one can be included from mods[3]
		The one remainder that repeats, ==> k/2 (because i == k-i => i = k/2)
	In contrast Odd k=5: (1+4), (2+3)
	'''

	# if k is even, include one element from mods[k/2] iff atleast one
	# element exists in the input list, with % == k/2
	if (k & 1 == 0):
		nd_subset_count += (1 if mods[k/2] > 0 else 0)

	return nd_subset_count


n, k = map(int, raw_input().split())
l = map(int, raw_input().split())
print get_max_non_divisible_subset(l, n, k)

