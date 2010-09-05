#!/usr/bin/python

''' Project Euler - Problem 4 '''

def isPalindrome(n):
	lst = list(str(n))
	rev = lst[:]
	rev.reverse()
	return (lst == rev)

def findPalindrome():
	pal = [0, 0, 0]
	for i in range(999, 99, -1):
		for j in range (999, 99, -1):
			if (isPalindrome (i*j)):
				newPal =  [(i*j), i, j]
				#print newPal
				if (newPal[0] > pal[0]):
					pal = newPal[:]
	return pal

print findPalindrome()
