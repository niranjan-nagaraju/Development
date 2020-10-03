'''
https://www.interviewbit.com/problems/substring-concatenation/

Substring Concatenation

You are given a string, S, and a list of words, L, that are all of the same length.

Find all starting indices of substring(s) in S that is a concatenation of each word in L exactly once and without any intervening characters.

Example :
	S: "barfoothefoobarman"
	L: ["foo", "bar"]
	You should return the indices: [0,9].
	(order does not matter).
'''


'''
Solution Outline:
	0. All words in L have the same number of letters, say l, and total number of words in L be nL
	1. Generate all substrings in S of length (nL*l)
		1.1 For each substring, s, split it into nL words of length l each
			1.1.1 Check if each of these words are found in s (in any order)
			1.1.2 If each word from the split are found in s, then capture the starting index of s.


Sample run:
	S: "barfoothefoobarman"
	L: ("bar", "foo")

	l = 3, nL = 2
	nL*l = 6
	=> Generate all substrings in S, of length 6
		['barfoo', 'arfoot', 'rfooth', 'foothe', 'oothef', 'othefo', 'thefoo', 'hefoob', 'efooba', 'foobar', 'oobarm', 'obarma', 'barman']
		=> For each of these substrings, s, Divide them into nL words, each of length l
			=> e.g. "barfoo" : ["bar", "foo"]
				Match all words in L in the split words list
				In ["bar", "foo"] we find all word in L and "barfoo: starts at 0
				And
				in "foobar", => ["foo", "bar"] -> at index 9

		return [0,9]
'''


from collections import defaultdict
class Solution:
	# @param S : string
	# @param L : tuple of strings
	# @return a list of integers
	def findSubstring(self, S, L):
		if not S or not L:
			return []

		lookup = defaultdict(lambda: [])
		word_len = len(L[0])
		substr_len = word_len * len(L)
		result = []

		# record frequencies of each word in L
		freqs = defaultdict(lambda:0)
		for x in L:
			freqs[x] += 1

		for i in xrange(len(S)-substr_len+1):
			substring = S[i:i+substr_len]

			freqs_copy = freqs.copy()

			# split the substring into len(L) parts
			for j in xrange(0, substr_len-word_len+1, word_len):
				word = substring[j:j+word_len]

				# Check if all words in L are part of 'substring'
				if word in freqs_copy:
					freqs_copy[word] -= 1
					if freqs_copy[word] == 0:
						del freqs_copy[word]

			if len(freqs_copy) == 0:
				# matched all words in L in the substring
				result.append(i)

		return result


if __name__ == '__main__':
	s = Solution()
	assert s.findSubstring("barfoothefoobarman", ("bar", "foo")) == [0,9]
	assert s.findSubstring("catbatatecatatebat", ("cat", "ate", "bat")) == [0,3,9]
	assert s.findSubstring("abcdababcd", ("ab", "ab", "cd")) == [0,2,4]
	assert s.findSubstring("abcdababcd", ("ab", "ab")) == [4]

