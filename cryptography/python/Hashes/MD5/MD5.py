''' 
 Implement MD5.
 Reference Wiki and RFC1321
 Reference Rivest's implementation from here -
	http://userpages.umbc.edu/~mabzug1/cs/md5/md5.html
''' 

# Memcopy specified bytes starting from the respective offsets
def memcpy (outBytes, outOffset, inBytes, inOffset, bytesLen):
	outBytes[outOffset : (outOffset + bytesLen)] = \
		inBytes [inOffset : (inOffset + bytesLen)]


# left rotate a 4-byte word by 'c'
def left_rotate (x, c):
	return ( x << c) | ( x >> (32-c) )


# Pack streams of 4-bytes into a 32-bit word; 
# len is assumed to a multiple of 4 (number of 'bytes' not words)
def pack (outp, inp, len):
	i = 0
	j = 0
	while ( j<len ):
		outp[i] = int(inp[j] | (inp[j+1] << 8) | (inp[j+2] << 16) | (inp[j+3] << 24))
		i += 1
		j += 4


# Unpack a 32-bit packed 4-byte sequence back into a stream of bytes;
# len is assumed to a multiple of 4 (number of 'bytes' not words)
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


# The MD5 state class
class MD5:
	# Initialize IV for this stream
	def __init__(self):
		# 64-byte buffer used to store residual bytes from a
		# previous MD5 state update
		# MD5 tranforms will not be called until
		# this buffer has atleast 64-bytes to process
		#  Useful when reading from a stream or a slow block device
		self.buffer = [0] * 64 
		self.buflen = 0 # number of residual bytes buffered

		# Two 32-bit length component to store
		# number of bits % 2^64 in the input stream
		self.lenwords = [0] * 2 # Will be unpacked to 8 bytes before adding to MD5 buffer

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

		# Store 16-bytes of MD5 digest
		self.digest = [0]*16


	# Update MD5 state from current input block
	def update(self, inText, inTextLen):
		# Number of bytes % 64
		#index = (self.lenwords[0] >> 3) & 0x3F;
		#index = self.buflen

		self.lenwords[0] += ((inTextLen << 3) & 0xFFFFFFFF)

		# Overflow, Update upper 32-bit length counter
		if (self.lenwords[0] < ((inTextLen << 3) & 0xFFFFFFFF)):
				self.lenwords[1] += 1

		# Update number of bits (hence the >> 29 (>>32 and <<3) in the upper 32-bit length counter 
		self.lenwords[1] += ((inTextLen >> 29 ) & 0xFFFFFFFF)

		# Utilize residual buffer from last update
		# and start filling current block following that
		partLen = 64 - self.buflen

		# We have atleast 64-bytes to process
		if (inTextLen >= partLen):
			memcpy (self.buffer, self.buflen, inText, 0, partLen)
			self.transform(self.buffer, 0)
	
			# Outstanding buffer utilized with last transformation
			self.buflen = 0 

			# Process in 64-byte blocks
			i = partLen
			while ( i + 63 < inTextLen ):
				self.transform (inText, i)
				i += 64

		else: # Current block + residual bytes from last update < 64 bytes
			i = 0

		# Buffer remaining input
		memcpy (self.buffer, self.buflen, inText, i, inTextLen-i)
		self.buflen += inTextLen-i


	# Complete computing MD5 by appending padding and length to residual bytes	
	# and updating MD5 state
	def finish (self):
		bits = [0] * 8
		unpack (bits, self.lenwords, 8) # store bit-length % 2**64

		# pad until 56 % 64
		index = ((self.lenwords[0] >> 3) & 0x3F)

		# e.g. 1: msglen = 112
		#			msglen % 64 == 48
		#			56 - 48 == 8
		#			ergo, msglen + 8 == 112+8 == 120 == 56 % 64
		#			pad_len = 8
		# e.g. 2: msglen == 124
		#			msglen % 64 == 60
		#			56 - 60 == -4
		#			<0, +64, == -4 + 64 == 60
		#			msglen + 60 == 124+60 == 184 == 56 % 64
		#			pad_len = 60
		pad_len = 56 - index 
		if (pad_len < 0):
			pad_len += 64

		padding = [0x80] + [0] * (pad_len-1)

		self.update (padding, pad_len)

		# Append length
		self.update (bits, 8)

		# Digest[16] = h0 append h1 append h2 append h3
		unpack(self.digest, self.h, 16)


	# MD5 Transform state based on current 64-byte block
	def transform(self, inText, inTextOffset):
		curr_block = inText[inTextOffset : inTextOffset+64] # current 64-byte block

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


	# Hexadecimal string digest to print() function
	def __str__(self):
		# Convert byte-by-byte to a hexadecimal string
		hex_digest = map (lambda x: ("%02x" %(x)), self.digest)

		# Join list of chars as string and return
		return "".join (hex_digest)
