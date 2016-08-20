'''
https://www.hackerrank.com/challenges/bigger-is-greater

Given a word 'w', rearrange the letters of 'w' to construct another word 's' in such a way that is lexicographically greater than 'w'. In case of multiple possible answers, find the lexicographically smallest one among them.

Input Format
The first line of input contains t, the number of test cases. Each of the next t lines contains w.

Output Format
For each testcase, output a string lexicographically bigger than 'w' in a separate line. In case of multiple possible answers, print the lexicographically smallest one, and if no answer exists, print no answer.

Sample Input
5
ab
bb
hefg
dhck
dkhc

Sample Output
ba
no answer
hegf
dhkc
hcdk

'''



'''
Next lexicographic permutation:
	Find largest index i such that array[i - 1] < array[i], pivot <- array[i-1], suffix: array[i .. (n-1)]
	Find largest index j such that j >= i and array[j] > array[i - 1]
    Swap array[j] and array[i - 1]
    Reverse the suffix starting at array[i]
'''

# Largest decreasing sequence from right -- 'suffix'
def largest_decreasing_sequence(l, n):
	i = n-1
	while (i >= 0) and (l[i] <= l[i-1]):
		i -= 1

	return  i


# Reverse l[i .. (n-1)]
def reverse(l, i, n):
	j = n-1
	while (i < j):
		l[i], l[j] = l[j], l[i]
		i += 1
		j -= 1

def next_lexicographic_permutation(l, n):
	i = largest_decreasing_sequence(l, n)
	if i <= 0:
		return False # No next permutation exists, this is the last permutation (lexicographically)

	pivot = l[i-1]

	# From R-L, find first element in 'suffix'
	j = n-1
	while (j >= i) and (l[j] <= pivot):
		j -= 1

	# Swap l[i-1] and l[j]
	l[i-1], l[j] = l[j], l[i-1]

	# Reverse the 'suffix'
	reverse(l, i, n)

	return True



n = int(raw_input())
for i in xrange(n):
	l = map(ord, raw_input().strip())
	ln = len(l)
	print ''.join(map(chr, l)) if next_lexicographic_permutation(l, ln) else 'no answer'

