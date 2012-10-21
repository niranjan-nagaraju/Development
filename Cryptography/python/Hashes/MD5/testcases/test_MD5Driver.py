from MD5Driver import *

def runTestCases ():
	test_inputs = [ "", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz", \
					"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", \
					"12345678901234567890123456789012345678901234567890123456789012345678901234567890", \
					"The quick brown fox jumps over the lazy dog" \
					] 

	for i in test_inputs:
		print MD5String (i)


runTestCases()
print MD5File("testcases/test_file")
print MD5File("testcases/Mrok.jpg")
