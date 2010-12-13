#!/usr/bin/python

from caesar_cipher import *

def test_enc_dec():
	print 'Plaintext: ',
	plaintext = raw_input()

	print 'Key: ',
	key = int(input())

	ciphertext = Caesar_cipher.encrypt(plaintext, key)
	dec_plaintext = Caesar_cipher.decrypt(ciphertext, key)

	print 'Ciphertext: {0}'.format(ciphertext)
	print 'Decrypted plaintext: {0}'.format(dec_plaintext)


if __name__ == "__main__":
	test_enc_dec()
