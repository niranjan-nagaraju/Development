#!/usr/bin/python

''' 
 Implement MD5.
 Reference Wiki and RFC1321
''' 

# left rotate a 4-byte word by 'c'
def left_rotate (x, c):
	return ( x << c) | ( x >> (32-c) )


# Encodes 4 bytes into a 32-bit word; len is assumed to a multiple of 4 (number of 'bytes' not words)
def pack (outp, inp, len):
	i = 0
	j = 0
	while ( j<len ):
		outp[i] = int(inp[j] | (inp[j+1] << 8) | (inp[j+2] << 16) | (inp[j+3] << 24))
		i += 1
		j += 4


# unpacks a 32-bit packed 4-byte sequence back; len is assumed to a multiple of 4 (number of 'bytes' not words)
def unpack (outp, inp, len):
	j = 0
	i = 0
	while ( j < len ):
		outp[j] = int(inp[i] & 0xff);
		outp[j+1] = int((inp[i]>>8) & 0xff);
		outp[j+2] = int((inp[i] >> 16) & 0xff);
		outp[j+3] = int((inp[i] >> 24) & 0xff);
		j += 4
		i += 1



class MD5:

	def __init__(self, inText, inTextLen):
		# per round shift amounts
		self.r =  \
			[7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22] + \
			[5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20] + \
			[4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23] + \
			[6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]

		
		# Round Constants [64, first 16 in round one next 16 in round two and so on until round 4]	
		#
		# Computed from binary integer part of the sines of integers (Radians) as constants:
		# for i from 0 to 63
		#    k[i] := floor(abs(sin(i + 1)) * (2 pow 32))
		# end for
		self.k = \
			[0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee] + \
			[0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501] + \
			[0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be] + \
			[0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821] + \
			[0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa] + \
			[0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8] + \
			[0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed] + \
			[0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a] + \
			[0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c] + \
			[0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70] + \
			[0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05] + \
			[0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665] + \
			[0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039] + \
			[0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1] + \
			[0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1] + \
			[0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]

		# Initialization vector
		self.h = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476]

		self.digest = [0]*16
		self.unpadded_text = inText[:]

		# Intext is a sequence of bytes
		self.inText = inText
		self.inTextLen = inTextLen

		self.preprocess()
		self.calculate()

	def preprocess(self):
		# Append 1 + m zeroes to the message
		# m s.t. message length becomes 448 mod 512 ergo 56 mod 64
		# In essence add 0x80 followed 
		# by (l+1) == 56 mod 64
		self.inText += [0x80]

		# e.g. 1: msglen = 112
		#			+1 == 113
		#			msglen % 64 == 49
		#			56 - 49 == 7
		#			ergo, msglen + 7 == 113+7 == 120 == 56 % 64
		# e.g. 2: msglen == 124
		#			+1 == 125
		#			msglen % 64 == 61
		#			56 - 61 == -5
		#			<0, +64, == -5 + 64 == 59
		#			msglen + 59 == 125+59 == 184 == 56 % 64
		num_zerobytes = 56 - (self.inTextLen + 1) % 64
		if (num_zerobytes < 0):
			num_zerobytes += 64

		# Add required zero bytes
		self.inText += [0] * num_zerobytes

		# Add unpadded length mod (2 ** 64) to message
		# Store number if 'bits' not bytes,
		# essentially (len * 8) % (2 ** 64)
		unpadded_len = self.inTextLen
		packed_8byte_len = [0] * 8

		# Split 8-byte length (in bits) into two 4-byte words and unpack them into list of 8-bytes
		# when stored in little endian
		# NOTE: 0xabcdef12 will appear byte-wise as 0x12 0xef 0xcd 0xab
		#		so a length of 1 => 0x08 00 00 00, 00 00 00 00 (0x08 == 08 == 1*8)
		#					   2 => 0x10 00 00 00, 00 00 00 00 (0x10 == 16 == 2*8)
		#					   3 => 0x18 00 00 00, 00 00 00 00 (0x18 == 24 == 3*8) ...
		# TODO: Chances of overflow?! FIX if found
		unpack ( packed_8byte_len,
				 [((unpadded_len << 3) & 0xFFFFFFFF), ((unpadded_len >> 29) & 0xFFFFFFFF)], 8)

		self.inText += packed_8byte_len 

		self.inTextLen += 1 + num_zerobytes + 8


	def calculate(self):
		# Process the message in successive 64-byte chunks
		j = 0
		while ( j<self.inTextLen ):
			curr_block = self.inText[j : (j+64)] # current 64-byte block

			# Encode 64-byte block into 16 4-byte words
			w = [0] * 16 
			pack (w, curr_block, 64)
			
			(a, b, c, d) = (self.h[0]&0xFFFFFFFF, self.h[1]&0xFFFFFFFF, self.h[2]&0xFFFFFFFF, self.h[3]&0xFFFFFFFF)
			
			for i in range(0, 64):
				if ( 0 <= i <= 15 ):
					f = ((b & c) | ((~b) & d)) & 0xFFFFFFFF
					g = i & 0xFFFFFFFF
				elif ( 16 <= i <= 31 ):
					f = ((d & b) | ((~d) & c)) & 0xFFFFFFFF
					g = ((5*i + 1) % 16) & 0xFFFFFFFF
				elif ( 32 <= i <= 47 ):
					f = (b ^ c ^ d) & 0xFFFFFFFF
					g = ((3*i + 5) % 16) & 0xFFFFFFFF
				else: # ( 48 <= i <= 63 )
					f = (c ^ (b | (~d))) & 0xFFFFFFFF
					g = ((7*i) % 16) & 0xFFFFFFFF
		
				temp = int(d) & 0xFFFFFFFF
				d = c & 0xFFFFFFFF
				c = b & 0xFFFFFFFF
				b = (b + left_rotate ( ((a + f + self.k[i] + w[g]) & 0xFFFFFFFF), self.r[i] )) & 0xFFFFFFFF
				a = temp & 0xFFFFFFFF

			# end for 

			# Add this chunk's hash to result so far
			self.h[0] = (self.h[0] + a) & 0xFFFFFFFF
			self.h[1] = (self.h[1] + b) & 0xFFFFFFFF
			self.h[2] = (self.h[2] + c) & 0xFFFFFFFF
			self.h[3] = (self.h[3] + d) & 0xFFFFFFFF

			j += 64 # Next 64-bit block
		# end while

		# Digest[16] = h0 append h1 append h2 append h3
		unpack(self.digest, self.h, 16)


	# Hexadecimal string digest to print() function
	def __str__(self):
		# Convert to byte-by-byte hexadecimal string
		hex_digest = map (lambda x: ("%02x" %(x)), self.digest)

		# Join list of chars as string and return
		return "".join (hex_digest)


	# Hexadecimal string digest 
	def printableDigest(self):
		return self.__str__()


	# Print (Text): <digest>
	def printFormattedDigest(self):
		print "("+ "".join(map(lambda x: chr(x), self.unpadded_text)) + "):",
		print self.__str__()


