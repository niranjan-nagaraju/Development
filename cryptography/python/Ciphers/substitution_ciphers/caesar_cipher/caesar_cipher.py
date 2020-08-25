#!/usr/bin/python

class CaesarCipher:
	def __init__(self, key):
		self.key = key

	def encrypt(self, plaintext):
		ciphertext = ""
		for p in plaintext:
			if (p.isalpha()):
				ciphertext += (chr (((ord(p.upper()) - ord('A') + self.key) % 26) + ord('A'))).upper()
			else:
				ciphertext += p

		return ciphertext

	def decrypt(self, ciphertext):
		plaintext = ""
		for c in ciphertext:
			if (c.isalpha()):
				plaintext += (chr (((ord(c.upper()) - ord('A') - self.key) % 26) + ord('A'))).lower()
			else:
				plaintext += c
	
		return plaintext

	@staticmethod
	def brute_force(ciphertext):
		print "Brute forcing Ciphertext", ciphertext
		for i in range(1, 26):
			print i, "\t", CaesarCipher(i).decrypt(ciphertext)

def main():
	cipher = CaesarCipher(3)
	t = "Meet me after the toga party"
	c = cipher.encrypt(t)
	p = cipher.decrypt(c)

	print 'Plaintext:', t
	print 'Ciphertext:', c
	print

	CaesarCipher.brute_force(c)

if __name__ == "__main__":
	main()
