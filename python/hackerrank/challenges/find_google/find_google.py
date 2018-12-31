'''
https://www.hackerrank.com/contests/find-google/challenges/find-google/problem

The word google can be spelled in many different ways.

E.g. google, g00gle, g0oGle, g<>0gl3, googl3, GooGIe etc...

Because

g = G

o = O = 0 = () = [] = <>

l = L = I

e = E = 3

That's the problem here to solve.

Input Format

Exactly one word, e.g. "G00gL3"

Constraints

Here are all possible cases for the letters of the word "google":

    g = G
    o = 0 = O = () = [] = <>
    l = L = I
    e = E = 3

Output Format

Return "True" or "False".

Whether there is a match or not.

Sample Input 0

google

Sample Output 0

True

Sample Input 1

g()()GI3

Sample Output 1

True
'''



#!/bin/python

import math
import os
import random
import re
import sys


def tokenize(string):
	tokenized = []
	i = 0
	while i in range(len(string)):
		try:
			if (string[i] == '(' and string[i+1] == ')') or (string[i] == '[' and string[i+1] == ']') or (string[i] == '<' and string[i+1] == '>'):
				tokenized.append('o')
				i += 1
			else:
				tokenized.append(string[i].lower())
			i += 1
		except IndexError:
			break

	return tokenized





if __name__ == '__main__':
	#string = raw_input().strip()

	#string = 'G()[]gIe'
	string = 'goocle'

	google = [['g'], ['o', '0'], ['o', '0'], ['g'], ['l', 'i'], ['e', '3']]

	tokenized_string = tokenize(string)
	#print tokenized_string

	n = len(tokenized_string)
	if n != 6:
		print False
		exit(0)

	for i in range(n):
		#print i, tokenized_string[i], google[i]
		if tokenized_string[i] not in google[i]:
			print False
			exit(0)

	print True


