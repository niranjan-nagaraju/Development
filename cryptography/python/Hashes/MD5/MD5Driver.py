from MD5 import MD5
import sys

class MD5String(MD5):
	def __init__(self, string):
		MD5.__init__(self)
		self.preimage = string

		# convert string to a list of ascii bytes
		inText = map( lambda x: ord(x), string )
		self.update(inText, len(inText))
		self.finish()

	def __str__(self):
		return MD5.__str__(self)


class MD5File(MD5):
	def __init__(self, filename=None):
		MD5.__init__(self)
		self.filename = filename

		self.filehandle = open(self.filename, "rb")

		for inString in iter(self.read1KB, ''):
			# Convert ascii to a list of bytes
			# TODO: Why do I need to do this when I am reading binary???
			curr_1k_block = map( lambda x: ord(x), inString )

			self.update(curr_1k_block, len(curr_1k_block))

		self.finish()

		self.filehandle.close()

	def read1KB(self):
		return self.filehandle.read(1024)
	
	def __str__(self):
		return MD5.__str__(self)
