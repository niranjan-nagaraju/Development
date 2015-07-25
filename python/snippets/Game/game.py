#!/usr/bin/python

import random
from ospd4 import *

computers_word = ''

def hasUniqueLetters(word):
	l = sorted(word)
	i = 0
	while i < len(word)-1:
		if l[i] == l[i+1]:
			return False

		i = i + 1

	return True


def getRandomWordFromDict():
	idx = random.randint(0, len(ospd4)-1)
	word = ospd4[idx]
	if not hasUniqueLetters(ospd4[idx]):
		word = getRandomWordFromDict()

	return word


def compute_a_b(word1, word2):
	i = 0
	(a, b) = (0, 0)
	
	for c in word1:
		try:
			idx = word2.index(c)
			if (idx != i):
				b = b + 1
			else:
				a = a + 1
		except ValueError:
			pass


		i = i + 1

	return (a, b)	



def get_user_guess(try_x):
	guess = raw_input('Guess #[%d]: ' % (try_x))
	if (not guess.isalpha()) or (len(guess) != 4) or (not hasUniqueLetters(guess)) or (not guess in ospd4):
		print 'Invalid input. Guess again!'
		guess = get_user_guess(try_x)

	return guess	



def start_game():
	print 'Ready when you are... '
	computers_word = getRandomWordFromDict()

	print 'Alright I thought of a word..'
	print 'You got 20 guesses!'


	i = 0
	while i < 20:
		guess = get_user_guess(i+1)
		(a, b) = compute_a_b(computers_word,guess)
		if a == 4:
			print 'BINGO!'
			return
		else:
			print guess, a, b


		i = i + 1

	print 'You lose! The word was', computers_word 	



if __name__ == '__main__':
	start_game()



