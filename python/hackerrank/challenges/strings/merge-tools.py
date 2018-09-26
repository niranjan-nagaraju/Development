'''
https://www.hackerrank.com/challenges/merge-the-tools/problem

split the string into n/k parts, de-dup individual split.
e.g.
AABCAAADA
3  

AB
CA
AD
'''

def uniq(s):
	u = ""
	for c in s:
		if c not in u:
			u += c

	return u

def merge_the_tools(s, k):
	i = 0
	for i in range(0, len(s)-k+1, k):
		print uniq(s[i:i+k])


if __name__ == '__main__':
	string, k = raw_input(), int(raw_input())
	merge_the_tools(string, k)
