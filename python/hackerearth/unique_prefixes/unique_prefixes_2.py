#+encoding: utf-8

'''
https://www.hackerearth.com/problem/algorithm/unique-prefixes/

Problem
Suppose in a list of n distinct words, no word is a prefix of another word. In that case, each word has a unique prefix too, often shorter than word, which can be used to distinctly represent the word among the list of n words. Find out the shortest unique prefix of each word.

Input Format:
	Input starts with a positive integer n indicating the number of words to follow, one in each line. A word in each line n lines.

Output Format:
	Output to have n lines, one line for each corresponding input word, with the shortest prefix of the word.

Constraints:
	1 ≤ n ≤ 10^6
	1 ≤ length of word ≤ 10^3
	1 ≤ sum of the number of characters of all the words ≤ 10^6

Sample Input
	6
	pesu
	guru
	suffix
	prefix
	pesit
	science
Sample Output
	pesu
	g
	su
	pr
	pesi
	sc
'''

'''
Solution Outline:
    1. Build a prefix map of the words, associating each prefix of a word with the word.
		e.g., abcd
		a*, ab*, abc*, abcd* -> abcd
	2. For each word, walk through its prefixes from shortest - longest until the prefix matches only 1 word.
	3. Return the prefix as its shhorortest unique prefix

Sample run:
    Input: [zebra, dog, duck, dove]

	prefix-map
	+ zebra: 
	z* -> 1
	ze* -> 1
	zeb* -> 1
	zebr* -> 1
	zebra* -> 1

	+ dog:
	d* -> 1
	do* -> 1
	dog* -> 1

	+ duck:
	d* -> 2
	du* -> 1
	duc* -> 1
	duck* -> 1

	+ dove:
	d* -> 3
	do* -> 2
	dov* -> 1
	dove* -> 1


shortest-unique-prefixes
lookup:
	word: zebra
	z* -> 1
	zebra: z

	word: dog
	d* -> 2
	do* -> 2
	dog* -> 1
	dog: dog

	word: duck
	d* -> 2
	du* -> 1
	duck: du

	word: dove
	d* -> 2
	do* -> 2
	dov* -> 1
	dove: dov

	return [z, dog, du, dov]
'''

from collections import defaultdict
class Solution:
	def shortest_unique_prefix(self, words):
		if len(words) < 2:
			yield words

		t = defaultdict(lambda: 0)
		for word in words:
			for i in xrange(1,len(word)+1):
				t[word[:i]] += 1

		for word in words:
			for i in xrange(1,len(word)+1):
				if t[word[:i]] == 1:
					break
			yield word[:i]

def basic_testcases():
	s = Solution()
	assert [_ for _ in s.shortest_unique_prefix(['zebra', 'dog', 'duck', 'dove']) ] ==\
			['z', 'dog', 'du', 'dov']

	assert [_ for _ in s.shortest_unique_prefix([ "zebra", "dog", "duck", "dot" ]) ] == \
			['z', 'dog', 'du', 'dot']
	assert [_ for _ in s.shortest_unique_prefix([ "bearcat", "bert" ]) ] == ['bea', 'ber']
	assert [_ for _ in s.shortest_unique_prefix(["pesu", "guru", "suffix", "prefix", "pesit", "science"]) ] == \
			["pesu", "g", "su", "pr", "pesi", "sc"]


if __name__ == '__main__':
	basic_testcases()
	n = int(input())
	words = [raw_input().strip() for _ in xrange(n)]
	s = Solution()
	for prefix in s.shortest_unique_prefix(words):
		print prefix

