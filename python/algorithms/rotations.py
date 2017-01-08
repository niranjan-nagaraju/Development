# Find if two strings are rotations of each other
# O(n) time complexity

def areRotations (str1, str2):
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

if __name__ == "__main__":
	main()
