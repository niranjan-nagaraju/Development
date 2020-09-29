'''
Decode URL-encoded strings to utf-8
'''

def decode_url(encoded_string):
	decoded = []

	i= 0
	while i < len(encoded_string):
		if encoded_string[i] == '%':
			# lookahead 2 characters
			# and read them as hex
			hex_value = int(encoded_string[i+1:i+3], 16)
			if chr(hex_value) == "'":
				# convert all single-quotes to double-quotes
				hex_value = ord('"')
			decoded.append(chr(hex_value))
			i += 2
		else:
			# regular character, copy as-is
			decoded.append(encoded_string[i])
		i += 1
	
	return b''.join(decoded)



if __name__ == '__main__':
	test_string = b'GET%20%2F%7B%22mac%22%3A%221A%3A2B%3A3C%3A4D%3A5E%3A6F%22%2C%20%27status%27%3Atrue%7D'
	assert decode_url(test_string) == b'GET /{"mac":"1A:2B:3C:4D:5E:6F", "status":true}'

	t2 = b'GET%20%2F%7B%22mac%22%3A%221A%3A2B%3A3C%3A4D%3A5E%3A6F%22%2C%20%27status%27%3Atrue%2C%20%22epoch%22%20%3A%2012345678%7D'
	assert decode_url(t2) == b'GET /{"mac":"1A:2B:3C:4D:5E:6F", "status":true, "epoch" : 12345678}'

	t3 = b'GET / {abc: "def"}'
	assert t3 == t3
