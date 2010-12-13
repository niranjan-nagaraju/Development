#!/usr/bin/python

from monoalphabetic_cipher import *

def test_enc_dec():
	letters = range(ord('a'), ord('z')+1)
	letters = map(chr, letters)

	print 'Plaintext: ',
	plaintext = raw_input()

	print 'Keystring: ',
	keymap = raw_input().lower()

	'''
	Create a keymap by using the letters in the key provided and pad it by 
	appending the rest of the alphabet that aren't in the key
	'''
	for k in keymap:
		letters.remove(k)
	
	for l in letters:
		keymap += l

	print 'Keymap: ', keymap

	m = MonoalphabeticCipher(keymap)
	ciphertext = m.encrypt(plaintext)
	dec_plaintext = m.decrypt(ciphertext)

	print 'Ciphertext: {0}'.format(ciphertext)
	print 'Decrypted plaintext: {0}'.format(dec_plaintext)


if __name__ == "__main__":
	test_enc_dec()
