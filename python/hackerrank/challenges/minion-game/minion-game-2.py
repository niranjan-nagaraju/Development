'''
https://www.hackerrank.com/challenges/the-minion-game/problem

Solution 1:
	Optimized version -
	  Number of substrings that begin with s[0] is n, where n = len(s)
		e.g., ABCDEF -> A AB ABC ABCD ABCDE ABCDEF
	  Similarly, substrings that begin with s[i] for any i<n, is n-i

	  Add #substrings that begin with s[i] to stuart/kevin depending on whether s[i] is
	  a consonant/vowel respectively
'''

def isVowel(c):
	vowels = "AEIOU"
	return c.upper() in vowels

def substr_game_scores(s):
	stuart = 0
	kevin = 0

	n = len(s)
	for i in range(n):
		# Count scores for words that begin with s[i]

		# Current substrings begin with a vowel, award scores to kevin
		if isVowel(s[i]):
			kevin += n-i
		else: # Consonant-prefixed substrings, add scores to stuart
			stuart += n-i
		
	if stuart > kevin:
		print 'Stuart', stuart
	elif stuart < kevin:
		print 'Kevin', kevin
	else:
		print 'Draw'


if __name__ == "__main__":
	s = raw_input().strip()
	substr_game_scores(s)



'''
Test Executions:
	BANANA
	Stuart 12

	EBE
	Kevin 4

	ABCDEF
	Stuart 13

	BEE
	Draw
'''
