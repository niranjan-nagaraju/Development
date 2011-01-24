#!/usr/bin/python

class PlayfairCipher:
	def __init__(self, original_key):
		self.original_key = original_key
		self.key_matrix = self.prepare_key_matrix(original_key) # Create a 5x5 lookup matrix

	# Create a 5x5 key matrix
	@staticmethod
	def prepare_key_matrix(original_key):
		alphabets = range(ord('a'), ord('z')+1)

		# Eliminate duplicate characters in key
		key = [] 
		[key.append(k) for k in original_key if k not in key]	# Eliminate duplicate characters in key

		key_matrix = [k.upper() for k in key ] + [chr(k).upper() for k in alphabets if (chr(k) not in key) and (chr(k).upper() != 'I') ]

		return key_matrix

def main():
	p = PlayfairCipher("mmonaarchyy")

	print 'Key Matrix: ', p.key_matrix


if __name__ == "__main__":
	main()
