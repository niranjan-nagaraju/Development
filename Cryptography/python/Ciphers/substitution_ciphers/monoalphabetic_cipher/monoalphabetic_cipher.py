#!/usr/bin/python

class MonoalphabeticCipher:
	def __init__(self, keymap):
		self.enc_key_map = self.generate_enc_key_map(keymap)	# Encryption lookup table
		self.dec_key_map = self.generate_dec_key_map(keymap)	# Decryption reverse-lookup table

	# Create a lookup table for encryption; Pre-store as uppercase
	@staticmethod
	def generate_enc_key_map(keymap):
		enc_key_map = []
		for i in keymap:
			enc_key_map.append(i.upper())

		return enc_key_map

	# Create a reverse lookup table for decryption
	@staticmethod
	def generate_dec_key_map(keymap):
		dec_key_map = range(0, 26) # Dummy place holders
		for i in range(0, 26):
			c = keymap[i]
			rev_index = ord(c.lower()) - ord('a')
			dec_key_map[rev_index] = chr(ord('a') + i)

		return dec_key_map

	def encrypt(self, plaintext):
		ciphertext = ""
		for p in plaintext:
			if p.isalpha():
				i = ord(p.lower()) - ord('a') # a:0, b:1 .. z:25
				ciphertext += self.enc_key_map[i]
			else:
				ciphertext += p

		return ciphertext

	def decrypt(self, ciphertext):
		plaintext = ""
		for c in ciphertext:
			if c.isalpha():
				i = ord(c.upper()) - ord('A') #A:0, B:1 .. Z:25
				plaintext += self.dec_key_map[i]
			else:
				plaintext += c

		return plaintext


	'''
	TODO:
	  Frequency analysis
	'''
	@staticmethod
	def freq_analysis(ciphertext):
		guessed_plaintext = ""
		return guessed_plaintext


def main():
	m = MonoalphabeticCipher("ZEBRASCDFGHIJKLMNOPQTUVWXY")

	print 'Enc Map: ', m.enc_key_map
	print 'Dec Map: ', m.dec_key_map

	plaintext = "Hello World"
	ciphertext = m.encrypt(plaintext)
	dec_plaintext = m.decrypt(ciphertext)

	print 'Encrypted Ciphertext: ', ciphertext
	print 'Decrypted Plaintext: ', dec_plaintext


if __name__ == "__main__":
	main()
