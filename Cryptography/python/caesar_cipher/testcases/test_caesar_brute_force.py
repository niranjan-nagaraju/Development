#!/usr/bin/python

from caesar_cipher import *

def test_brute_force(ciphertext):
	CaesarCipher.brute_force(ciphertext)

if __name__ == "__main__":
	print 'Ciphertext: ',
	ciphertext = raw_input()

	test_brute_force(ciphertext)
