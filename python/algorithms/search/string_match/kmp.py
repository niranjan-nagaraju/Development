'''
Knuth-Morris-Pratt algorithm to find pattern in text

KMP computes the LPS (Longest-Prefix which is also a Suffix) table for the pattern string.
The LPS table is then used to measure where to resume string matching from in the event of a mismatch between pattern[i] and text[j]


LPS array:
	"AAAA":
	   A  A  A  A
	  [0, 1, 2, 3]

	"ABCDE":
	   A  B  C  D  E
	  [0, 0, 0, 0, 0]

	"AABAACAABAA":
	   A  A  B  A  A  C  A  A  B  A  A
	  [0, 1, 0, 1, 2, 0, 1, 2, 3, 4, 5]

	"AAACAAAAAC":
	   A  A  A  C  A  A  A  A  A  C
	  [0, 1, 2, 0, 1, 2, 3, 3, 3, 4] 

	"AAABAAA":
	   A  A  A  B  A  A  A
	  [0, 1, 2, 0, 1, 2, 3] 
'''


'''
Calculate LPS array:
Sample run:
       0  1  2  3  4  5  6  7  8  9
  pat: A  A  A  C  A  A  A  A  A  C
      [0, 1, 2, 0, 1, 2, 3, 3, 3, 4] 

	lps: [0]
	lp: 0

	i: 1
	pat[i] == 'A' == A[lp]
	lp = 1
	lps[1] = 1
	lps: [0, 1]

	i: 2
	pat[i] == 'A' == A[lp]
	lp = 2
	lps[2] = 2
	lps: [0, 1, 2]

	i: 3
	pat[i] == 'C' != A[lp]
	lp = lps[lp-1] == lps[1] == 1
	--
	pat[i] == 'C' != A[lp]
	lp = lps[lp-1] = lps[0] = 0
	-- 
	pat[i] == 'C' != A[lp]
	lp == 0
	=> lps[3] = 0
	lps: [0, 1, 2, 0]

	i: 4
	pat[i] == 'A' == pat[lp]
	lp = 1
	lps[4] = 1
	lps: [0, 1, 2, 0, 1]

	i: 5
	pat[i] == 'A' == pat[lp]
	lps = 2
	lps[5] = 2
	lps: [0, 1, 2, 0, 1, 2]

	i: 6
	pat[i] == 'A' == pat[lp]
	lps = 3
	lps[6] = 3
	lps: [0, 1, 2, 0, 1, 2, 3]

	i: 7
	pat[i] == 'A' != pat[lp] ('C')
	lp = lps[lp-1] = lps[2] = 2
	--
	pat[i] == 'A' == pat[lp]
	lp = 3
	lps[7] = 3
	lps: [0, 1, 2, 0, 1, 2, 3, 3]

	i: 8
	pat[i] == 'A' != pat[lp] ('C')
	lp = lps[lp-1] = lps[2] = 2
	--
	pat[i] == 'A' == pat[lp]
	lp = 3
	lps[8] = 3
	lps: [0, 1, 2, 0, 1, 2, 3, 3, 3]

	i: 9
	pat[i] == 'C' == pat[lp]
	lp = 4
	lps[9] = 4
	lps: [0, 1, 2, 0, 1, 2, 3, 3, 3, 4]
'''
def calculate_lps(pat):
	lp = 0 # longest prefix-suffix found so far

	# lps[0] is 0, start from 1
	i = 1

	lps = [0]*len(pat)
	while i < len(pat):
		if pat[i] == pat[lp]:
			lp += 1
			lps[i] = lp
			i += 1
		else:
			# Longest prefix trail ends
			# pat[i] != pat[lp]
			# Backtrack longest prefix by lps[lp-1]
			# incase there are other prefixes that might match with pat[lp]
			if lp != 0:
				lp = lps[lp-1]
			else:
				lps[i] = 0
				i += 1

	return lps



'''
Find and return the index of the first pattern match in text
'''
def kmp_find(text, pat):
	m = len(pat)
	n = len(text)
	lps = calculate_lps(pat)


	i = j = 0
	while i < n:
		if text[i] == pat[j]:
			i += 1
			j += 1

		if j == m:
			# Matched pattern completely
			return i-j

		elif i < n and text[i] != pat[j]:
			if j != 0:
				# backtrack to see if there are other prefixes
				# which aligns with pat[j]
				j = lps[j-1]
			else:
				# Start matching from text[i+1]
				i += 1

	return -1


'''
Find and return the indices of all pattern matches in text
'''
def kmp_find_all(text, pat):
	m = len(pat)
	n = len(text)
	lps = calculate_lps(pat)
	matches = []

	i = j = 0
	while i < n:
		if text[i] == pat[j]:
			i += 1
			j += 1

		if j == m:
			# Matched pattern completely
			matches.append(i-j)
			j = lps[j-1]

		elif i < n and text[i] != pat[j]:
			if j != 0:
				# backtrack to see if there are other prefixes
				# which aligns with pat[j]
				j = lps[j-1]
			else:
				# Start matching from text[i+1]
				i += 1

	return matches


if __name__ == '__main__':
	assert kmp_find("abcdef", "bcd") == 1
	assert kmp_find("abcdef", "bce") == -1
	assert kmp_find("abcdabcxabcyef", "abcy") == 8
	assert kmp_find("THIS IS A TEST TEXT", "TEST") == 10
	assert kmp_find("THIS IS A TEST TEXT", "TEXT") == 15
	assert kmp_find("THIS IS A TEST TEXA", "TEXT") == -1

	assert kmp_find_all("abcdabcxabcyef", "abc") == [0, 4, 8]
	assert kmp_find_all("THIS IS A TEST TEXT", "TEXT") == [15]
	assert kmp_find_all("THIS IS A TEST TEST", "TEST") == [10, 15]
	assert kmp_find_all("THIS IS A TEST TEST", "TEXT") == []

