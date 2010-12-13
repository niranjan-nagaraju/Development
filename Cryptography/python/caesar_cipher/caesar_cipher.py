#!/usr/bin/python

class Caesar_cipher:
	@staticmethod
	def encrypt(plaintext, key):
		ciphertext = ""
		for p in plaintext:
			if (p.isalpha()):
				ciphertext += (chr (((ord(p.upper()) - ord('A') + key) % 26) + ord('A'))).upper()
			else:
				ciphertext += p

		return ciphertext

	@staticmethod
	def decrypt(ciphertext, key):
		plaintext = ""
		for c in ciphertext:
			if (c.isalpha()):
				plaintext += (chr (((ord(c.upper()) - ord('A') - key) % 26) + ord('A'))).lower()
			else:
				plaintext += c
	
		return plaintext

	@staticmethod
	def brute_force(ciphertext):
		print "Brute forcing Ciphertext", ciphertext
		for i in range(1, 26):
			print i, "\t", Caesar_cipher.decrypt(ciphertext, i)

def main():
	t = "Meet me after the toga party"
	c = Caesar_cipher.encrypt(t, 3)
	p = Caesar_cipher.decrypt(c, 3)

	print 'Plaintext:', t
	print 'Ciphertext:', c
	print

	Caesar_cipher.brute_force(c)

if __name__ == "__main__":
	main()
