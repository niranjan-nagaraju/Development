#!/usr/bin/python

from TriangleWords_42 import *

# 14 is the max characters
# so max triangle word can be 14 * 26 = 364 < sigma(26)

def get_triangleNumbers ():
	i = 1
	tri_list = []
	running_sum = 0
	while ( i <= 26 ):
		running_sum += i
		tri_list.append(running_sum)

		i += 1

	return tri_list

def sum_of_chars (word):
	sum_chars = 0
	for i in word:
		sum_chars += (ord(i) - ord('A') + 1)
	
	return sum_chars

def addifTriangleWord (word, triangleNums):
	if sum_of_chars(word) in triangleNums:
		return 1

	return 0

triangleNums = get_triangleNumbers()
print reduce ((lambda x, y: x + addifTriangleWord(y, triangleNums)), inWords, 0)
# 162
