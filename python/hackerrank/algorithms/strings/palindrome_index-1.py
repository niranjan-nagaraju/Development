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

def is_palindrome(s, n):
	i = 0
	j = n-1
	while i < j:
		if (s[i] != s[j]):
			return False

		i += 1
		j -= 1

	return True

# remove 1 char at a time from S and check if its a palindrome
def remove_char_and_test(s, n):
	for i in xrange(n):
		if is_palindrome(s[:i] + s[i+1:], n-1):
			return i

	return -1



t = int(raw_input())
for i in xrange(t):
	s = raw_input().strip()

	print remove_char_and_test(s, len(s))

