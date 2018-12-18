'''
Find if two strings are rotations of each other
O(n) time complexity

Solution:
	Append str1 to itself, then check if str2 is a substring within (str1+str1)
'''

def areRotations (str1, str2):
	# Compare lengths of the two strings 
	# to avoid false-positives
	# Else, str2 can be a substring of str1,
	#    e.g., str1: "abcad", str2: "cad"
	# *OR* str1 a substring of str2
	#    e.g., str1: "abcad", str2: "dabcad"
	# and we might return True incorrectly
	if len(str1) != len(str2):
		return False

	str1 += str1
	try:
		str1.index(str2) 
	except ValueError:
		return False

	return True


''' Testcases '''
def main():
	assert areRotations ("abc", "bca") == True
	assert areRotations ("abc", "cab") == True
	assert areRotations ("abc", "cba") == False
	assert areRotations ("waterbottle", "erbottlewat") == True
	assert areRotations("abcad", "cadab") == True

if __name__ == "__main__":
	main()
