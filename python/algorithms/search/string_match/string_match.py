'''
Find pattern in text.

Brute-force string match
   Let size(text) == n, size(pattern) == m  {m <=n}
   Align pattern[0..j] with text[i..i+m]  {i: 0<=i<n}
     If there's no complete match, start over with text[i+1], ...
'''


'''
Find and return the index of the first pattern match in text
'''
def find(text, pat):
	n = len(text)
	m = len(pat)
	for i in xrange(n-m+1):
		j = 0
		while j < m:
			if pat[j] != text[i+j]:
				break
			j += 1
		if j == m:
			return i

	return -1



'''
Find and return the indices of all pattern matches in text
'''
def find_all(text, pat):
	n = len(text)
	m = len(pat)
	matches = []
	for i in xrange(n-m+1):
		j = 0
		while j < m:
			if pat[j] != text[i+j]:
				break
			j += 1
		if j == m:
			matches.append(i)
		
	return matches



if __name__ == '__main__':
	assert find("abcdef", "bcd") == 1
	assert find("abcdef", "bce") == -1
	assert find("abcdabcxabcyef", "abcy") == 8
	assert find("THIS IS A TEST TEXT", "TEST") == 10
	assert find("THIS IS A TEST TEXT", "TEXT") == 15
	assert find("THIS IS A TEST TEXA", "TEXT") == -1

	assert find_all("abcdabcxabcyef", "abc") == [0, 4, 8]
	assert find_all("THIS IS A TEST TEXT", "TEXT") == [15]
	assert find_all("THIS IS A TEST TEST", "TEST") == [10, 15]
	assert find_all("THIS IS A TEST TEST", "TEXT") == []

