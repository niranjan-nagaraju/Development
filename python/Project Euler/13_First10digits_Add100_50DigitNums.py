#!/usr//bin/python

''' Project Euler - Problem 13 '''

i = 0
sum = 0
while i<100:
	e = int(input())
	e = str(e)[0:11]
	sum = sum + int(e)
	i = i + 1

s = str(sum)[0:10]
s = int(s)

print s

'''
Solution:
$ cat 13.data | python 13_First10digits_Add100_50DigitNums.py 
5537376230
'''
