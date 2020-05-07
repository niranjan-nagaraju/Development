'''
https://www.interviewbit.com/problems/longest-common-prefix/

Longest Common Prefix

Given the array of strings A,
you need to find the longest string S which is the prefix of ALL the strings in the array.

Longest common prefix for a pair of strings S1 and S2 is the longest string S which is the prefix of both S1
and S2.

For Example, longest common prefix of "abcdefgh" and "abcefgh" is "abc".

nput Format

The only argument given is an array of strings A.
Output Format

Return longest common prefix of all strings in A.
For Example

Input 1:
    A = ["abcdefgh", "aefghijk", "abcefgh"]
Output 1:
    "a"
Explanation 1:
	Longest common prefix of all the strings is "a".

Input 2:
    A = ["abab", "ab", "abcd"];
Output 2:
    "ab"
Explanation 2:
	Longest common prefix of all the strings is "ab".
'''

'''
Solution Outline:
	Observations: The longest common prefix only decreases(/or stays the same) with the introduction of each new word, never increases.
	E.g, 
	  w1: "abcde" , lcp initially is 5
	  w2: "abcd", lcp is now 4

	This is true regardless of the order in which the words arrive.
	  w1: "abcd" , lcp initially is 4
	  w2: "abcde", lcp stays at 4

	0. Mimic a Trie
	1. Use initial word, word0, as the base LCP length
	   Set LCP to len(word0)
	2. For each word after word0, compare with word0 for LCP-length only until they diverge.
	   Update the length of the path until they diverge, as the new LCP.
	    for e.g,
		  w0: "abcd"
		  Trie:
            a
           /	
          b 	
         /
        c
       /
      d

      LCP = 4

	  w1: "abef"
	  At 'e' w1 diverges from word0
	  LCP = 2

	  w3: "abcf"
	  Compare only first 2(=LCP) characters from w3 with w0 and see if they diverge.
	  They dont. => return 2 as LCP

	  w4: "axdfghjghkh"
	  Compare only first 2(=LCP) characters from w4 with w0 and see if they diverge.
	  At 'x' diverges, w4 diverges
	  => return 1 as the LCP

	3. Return the last updated LCP
'''
class Solution:
	def find_longest_common_prefix(self, A):
		# Compare max_len characters in w1 and w2
		# sequentially and return the first index
		# where they diverge
		def find_diverging_point(w1, w2, max_len):
			n1 = len(w1)
			n2 = len(w2)

			i = 0
			while i < max_len and i < n1 and i < n2:
				if w1[i] != w2[i]:
					break
				i += 1
			return i


		if len(A) == 0 or len(A[0]) == 0:
			return 0

		lcp = len(A[0])

		for i in xrange(1, len(A)):
			word = A[i]
			lcp = find_diverging_point(A[0], word, lcp)

		return A[0][:lcp]



if __name__ == '__main__':
	s = Solution()
	assert s.find_longest_common_prefix(["abcd", "abce", "abxf"]) == "ab"
	assert s.find_longest_common_prefix(["abxf", "abcd", "abce"]) == "ab"
	assert s.find_longest_common_prefix(["abcd", "abce", "axf"]) == "a"
	assert s.find_longest_common_prefix(["abcd", "abce", "f"]) == ""
	assert s.find_longest_common_prefix(["abcd", "abce", ""]) == ""
	assert s.find_longest_common_prefix(["abcd", "abcde", ""]) == ""

