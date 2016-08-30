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
				# A second mismatch is not expected
				# => S is not going to be a palindrome even if we remove a character

				return -1

			if (s[i+1] == s[j]):
				idx_to_remove = i
				i += 1
				removed_char = True
			elif (s[j-1] == s[i]):
				idx_to_remove = j
				j -= 1
				removed_char = True
			else:
				# None of (i,j), (i+1, j), (i, j-1) match 
				# => cant be made into a palindrome removing just one character

				print 'case 3', i, j, s[i], s[i+1], s[j-1], s[j]
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
failure cases repro:
	[01:29:40 nnagaraj strings]$ python palindrome_index.py 
	1
	cwnnwcw
	case 3 2 5 n n w c
	-1

	[01:38:34 nnagaraj strings]$ python palindrome_index.py 
	1
	cwwcw
	-1

'''
