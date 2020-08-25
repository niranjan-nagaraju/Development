from MD5_naive import MD5

def run_test (inText):
	inText = map( lambda x: ord(x), inText )
	return MD5(inText, len(inText)).printableDigest()


if __name__ == '__main__':
	assert run_test("") == "d41d8cd98f00b204e9800998ecf8427e"
	assert run_test("a") == "0cc175b9c0f1b6a831c399e269772661"
	assert run_test("abc") == "900150983cd24fb0d6963f7d28e17f72"
	assert run_test("message digest") == "f96b697d7cb7938d525a2f31aaf161d0"
	assert run_test("abcdefghijklmnopqrstuvwxyz") == "c3fcd3d76192e4007dfb496cca67e13b"
	assert run_test("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == "d174ab98d277d9f5a5611c2c9f419d9f"
	assert run_test("12345678901234567890123456789012345678901234567890123456789012345678901234567890") == "57edf4a22be3c955ac49da2e2107b67a"
	assert run_test("The quick brown fox jumps over the lazy dog") == "9e107d9d372bb6826bd81d3542a419d6"


