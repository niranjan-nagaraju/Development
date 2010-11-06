#!/usr/bin/python

def isPalindrome (text):
	return text == text[::-1]

text = raw_input()
print isPalindrome(text.upper())
