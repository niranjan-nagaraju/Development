'''
https://www.interviewbit.com/problems/compare-version-numbers/

Compare Version Numbers

Compare two version numbers version1 and version2.
	If version1 > version2 return 1,
	If version1 < version2 return -1,
	otherwise return 0.

You may assume that the version strings are non-empty and contain only digits and the . character.

The . character does not represent a decimal point and is used to separate number sequences.
For instance, 2.5 is not "two and a half" or "half way to version three", it is the fifth second-level revision of the second first-level revision.

Here is an example of version numbers ordering:
	0.1 < 1.1 < 1.2 < 1.13 < 1.13.4
'''

'''
Solution Outline:
	1. Split the version strings with '.' as delimiters.
	2. Lexicographically compare individual split field.
'''
class Solution:
	def lexicographic_compare(self, s1, s2):
		i = j = 0
		while i<len(s1) and s1[i] == '0':
			i += 1

		while j<len(s2) and s2[j] == '0':
			j += 1

		while i<len(s1) and j<len(s2) and s1[i] == s2[j]:
			i += 1
			j += 1

		# s1 matches s2 completely
		if i==len(s1) and j==len(s2):
			return 0

		# ran out of s1
		# '123' vs '1234'
		if i==len(s1):
			return -1

		# ran out of s2
		# '1234' vs '123'
		if j==len(s2):
			return 1

		# First differing character in s1 vs s2
		if (len(s1)-i) > (len(s2)-j):
			# s1's remaining characters are of greater length
			# for e.g.
			# '1234' vs '124'
			return 1
		elif (len(s1)-i) < (len(s2)-j):
			# s2's remaining characters are of greater length
			# for e.g.
			# '125' vs '1234'
			return -1
		else: # (len(s1)-i) == (len(s2)-j)
			# s1 and s2 have the same number of trailing characters to compare
			# but we are sure s1[i] != s2[j]
			# so return -1/+1
			return (-1 if (s1[i] < s2[j]) else +1)


		
	def compare_versions(self, A, B):
		vA = A.split('.')
		vB = B.split('.')

		if len(vB) < len(vA):
			# Fill vB with 0s at the end to match vA's length
			vB += ['0'] * (len(vA)-len(vB))
		else:
			# Fill vA with 0s at the end to match vB's length
			vA += ['0'] * (len(vB)-len(vA))

		i = j = 0
		# skip matching fields
		while i < len(vA) and j<len(vB)  and vA[i] == vB[j]:
			i += 1
			j += 1

		# All version fields match
		if i == len(vA):
			return 0

		# otherwise, we have found the first field where the versions differ
		return self.lexicographic_compare(vA[i], vB[j])



if __name__ == '__main__':
	s = Solution()
	assert s.compare_versions('44447', "5.168") == 1
	assert s.compare_versions('1', '1.0.0') == 0
	assert s.compare_versions('1.2.02', '1.2.3') == -1
	assert s.compare_versions('1.12.1', '1.13') == -1
	assert s.compare_versions('1.14.1.5', '1.14.1') == 1
	assert s.compare_versions('1.14.1.5', '1.20.1') == -1
	assert s.compare_versions('1.14.30', '1.14.3') == 1
	assert s.compare_versions('01', '1') == 0 

