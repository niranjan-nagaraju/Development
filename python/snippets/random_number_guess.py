from random import randint

def guess():
	iThoughtOf = randint(1,256)
	while True:
		print 'Guess: ',
		guessed = int(input())
		if guessed == iThoughtOf:
			print 'Bingo!, Get yourself a cookie.'
			break
		elif guessed < iThoughtOf:
			print 'Too Low'
		else:
			print 'Too high'


if __name__ == "__main__":
	guess()


