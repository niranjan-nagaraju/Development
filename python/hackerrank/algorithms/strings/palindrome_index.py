'''
https://www.hackerrank.com/challenges/palindrome-index

Given a string, S, of lowercase letters, determine the index of the character whose removal will make S a palindrome. If S is already a palindrome or no such character exists, then print -1. There will always be a valid solution, and any correct answer is acceptable. For example, if S="bcbc", we can either remove 'b' at index 0 or 'c' at index 3.

Output Format
Print an integer denoting the zero-indexed position of the character that makes S not a palindrome; if S is already a palindrome or no such character exists, print -1.

Sample Input
3
aaab
baa
aaa

Sample Output
3
0
-1

'''

# Make 's' a palindrome removing one character that breaks symmetry
def make_palindrome(s, n):
	'''
	While checking for a palindrome, a[i] <-> a[j], i->0,1,2,..., j<-n-1,n-2,..,
	One character breaks the symmetry (e.g. racBecar).
	"Remove" that character when a[i] and a[j] doesn't match.
	Essentially try a[i],a[i+1] vs a[j],a[j-1]
	'''
	i = 0
	j = n-1
	removed_char = False # We haven't removed a char yet
	idx_to_remove = -1
	while True:
		if (s[i] != s[j]):
			if (removed_char == True):
				# We have already removed a character to try and match
				# A second mismatch means we probably have to backtrack

				return -1

			# First mismatch here ..XabX..
			# any of 'a' or 'b' can be removed to make it a palindrome
			if (i + 1) == j:
				return i

			# check the next two elements as well so we don't assume
			# removing leftmost or rightmost will automatically
			# make it a palindrome
			# e.g. cwwcw => removing c at index 0 wont make it a palindrome
			if (s[i+1] == s[j]) and (s[i+2] == s[j-1]):
				idx_to_remove = i
				i += 1
				removed_char = True
			elif (s[j-1] == s[i]) and (s[j-2] == s[i+1]):
				idx_to_remove = j
				j -= 1
				removed_char = True
			else:
				# None of (i,j), (i+1, j), (i, j-1) match 
				# => cant be made into a palindrome removing just one character

				return -1

		if (i >= j):
			break

		i += 1
		j -= 1

	return idx_to_remove


t = int(raw_input())
for i in xrange(t):
	s = raw_input().strip()
	print make_palindrome(s, len(s))


'''
failure cases pass now:
	[22:30:52 nnagaraj strings]$ python palindrome_index.py 
	1
	cwwcw
	4
	[22:37:13 nnagaraj strings]$ python palindrome_index.py 
	1
	cwnnwcw
	6
	[22:37:25 nnagaraj strings]$ python palindrome_index.py 
	1
	bacb
	1
'''
