#!/usr/bin/python

class MonoalphabeticCipher:
	def __init__(self, keymap):
		self.enc_key_map = self.generate_enc_key_map(keymap)
		self.dec_key_map = self.generate_dec_key_map(keymap)

	@staticmethod
	def generate_enc_key_map(keymap):
		enc_key_map = []
		for i in keymap:
			enc_key_map.append(i.upper())

		return enc_key_map

	@staticmethod
	def generate_dec_key_map(keymap):
		dec_key_map = range(0, 26) # Dummy place holders
		for i in range(0, 26):
			c = keymap[i]
			rev_index = ord(c.lower()) - ord('a')
			dec_key_map[rev_index] = chr((ord('a') + i))

		return dec_key_map

	def encrypt(plaintext):
		ciphertext = []


	'''
	TODO:
	  Frequency analysis
	'''

def main():
	m = MonoalphabeticCipher("ZEBRASCDFGHIJKLMNOPQTUVWXY")

	print 'Enc Map: ', m.enc_key_map
	print 'Dec Map: ', m.dec_key_map

	print 'Enter text: '
	p = raw_input()
	c = m.encrypt(p)
	dec_p = m.decrypt(c)

	print '

