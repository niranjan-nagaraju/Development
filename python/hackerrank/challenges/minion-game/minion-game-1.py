'''
https://www.hackerrank.com/challenges/the-minion-game/problem

Solution 1:
	Brute-force approach::
	  Generate all substrings that begin with the first letter, add #substrings to either stuart/kevin based on if letter is consonant/vowel
	  Repeat for all following letters in the string.
'''

def isVowel(c):
	vowels = "AEIOU"
	return c.upper() in vowels

def substr_game_scores(s):
	stuart = 0
	kevin = 0

	n = len(s)
	for i in range(0, n):
		score_for_current_prefix = 0
		for j in range(1, n-i+1):
			# All substrings that begin with s[i]
			#print s[i:j+i],
			
			# Count scores for words that begin with s[i]
			score_for_current_prefix += 1

		# Current substrings begin with a vowel, award scores to kevin
		if isVowel(s[i]):
			kevin += score_for_current_prefix
		else: # Consonant-prefixed substrings, add scores to stuart
			stuart += score_for_current_prefix

		#print

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

	BEE
	Draw

	ABCDEF
	Stuart 13
'''	
