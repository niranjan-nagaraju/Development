#encoding: utf-8

from ospd5 import ospd5


# Legend:
#  -: missing letter
#  +: letter in right place
#  x: letter in wrong place


# e.g,
# guess:  A U D I O
# score:  - + x - -
# filter word as candidate if word does not have any of A, I, or O
#   BEAST: false
#   HYMNS: true
def filter_empty( word, guess, score ):
	for i,x in enumerate(score):
		if x == '-' and guess[i] in word:
			return False
	return True
	
# e.g,
# guess:  N I C H E
# score:  x + x - +
# filter word as candidate if all letters at right positions in guess are
# found in word at the same positions
#   BEING: false
#   BINGE: true
def filter_rightly_placed( word, guess, score ):
	for i,x in enumerate(score):
		if x == '+' and guess[i] != word[i]:
			return False
	return True

# e.g,
# guess:  N I C H E
# score:  x + x - +
# filter word as candidate if letters at wrong positions in guess are
# all present in different positions in word.
#   CINGE: true
#   NACHO: false
def filter_wrongly_placed( word, guess, score ):
	matches = []
	for i,x in enumerate(score):
		if x == 'x':
			# If there are duplicates in the 'guess', skip letters that are in the right place
			# e.g,
			#   guess: PADDY --x+-
			#   word:  ABODE
			#   skip lookup for position 4, so word.find('D')  returns []
			positions = [idx for idx, l in enumerate(word) if l == guess[i] and score[idx] != '+' ]
			#print(positions, i,  guess[i], [idx for idx, l in enumerate(word) if l == guess[i]])
			if positions and i not in positions:
				# Both word and guess have the wrongly-positioned letter at different positions
				matches.append(True)
	return (matches!=[]) and all(matches) and len(matches) == score.count('x')





class WordleGuesser:
	def __init__(self):
		self.candidates = set(ospd5)

	# filter out words from candidate
	# based on (guess,score)
	def filter_out( self, guess, score ):
		filtered_candidates = self.candidates.copy()
		for word in self.candidates:
			if '-' in score:
				if not filter_empty( word, guess, score ):
					filtered_candidates.remove( word )
		self.candidates = filtered_candidates.copy()
		#print( '1:' , sorted(candidates ))
		#print( len(candidates) )

		for word in self.candidates:
			if '+' in score:
				if not filter_rightly_placed( word, guess, score ):
					filtered_candidates.remove( word )
		self.candidates = filtered_candidates.copy()
		#print( '2:' , sorted(candidates ))
		#print( len(candidates) )

		for word in self.candidates:
			if 'x' in score:
				if not filter_wrongly_placed( word, guess, score ):
					filtered_candidates.remove( word )
		self.candidates = filtered_candidates


	def add_guess( self, guess, score ):
		emoji_mapping = { 'x': '🟨', '-': '⬛', '+': '🟩' }
		print(' Adding:', guess, ''.join([emoji_mapping[x] for x in score]))
		self.filter_out(guess.lower(), score.lower())

	def print_candidates( self ):
		print('Number of candidates: ', len(self.candidates) )
		print( 'candidates: ')
		print(sorted(self.candidates))


def run_tests():
	assert not filter_empty( 'beast', 'audio', '-+x--' )
	assert filter_empty( 'hymns', 'audio', '-+x--' )

	assert not filter_rightly_placed( 'being', 'niche', 'x+x-+' )
	assert filter_rightly_placed( 'binge', 'niche', 'x+x-+' )

	assert filter_wrongly_placed( 'cinge', 'niche', 'x+x-+' )
	assert not filter_wrongly_placed( 'nacho', 'niche', 'x+x-+' )
	assert filter_wrongly_placed('those', 'stern', 'xxx--')

	w = WordleGuesser()
	w.add_guess('stern', 'xxx--')
	w.add_guess('audio', '----x')
	w.print_candidates()

	w2 = WordleGuesser()
	w2.add_guess('audio', '-+x--')
	w2.print_candidates()
	w2.add_guess('irons', '-----')
	w2.print_candidates()
	w2.add_guess('stern', '-----')
	w2.print_candidates()


import sys
if __name__ == '__main__':
	#run_tests()
	assert not filter_empty( 'rupee', 'ruler', '++-+-') #????
	assert filter_rightly_placed( 'rupee', 'ruler', '++-+-')
	w = WordleGuesser()
	print('Legend: +: green, x: orange, -: blank')
	guesses_scores = [l.strip().split() for l in sys.stdin]
	#print('Sample Input: WORDS +-x+-') 
	for guess,score in guesses_scores:
		assert ( len(guess) == 5 and len(score) == 5 and guess.isalpha() ), "Incorrect Input format, Need WORDS +-x+-"
		w.add_guess(guess, score)
	w.print_candidates()

