# Find if two strings are anagrams of each other
# O(n) complexity

def areAnagrams(str1, str2):
	ascii_table = [0]*128;

	for s in str1:
		if not s.isspace():
			ascii_table[ord(s.lower())] += 1

	for s in str2:
		if not s.isspace():
			ascii_table[ord(s.lower())] -= 1

	for i in range(0, 128):
		if ascii_table[i] != 0:
			return False
	
	return True


''' Testcases '''

def main():
	assert areAnagrams("Hamlet", "Amleth") == True
	assert areAnagrams("Hello", "Hollo") == False
	assert areAnagrams ("Tom Marvolo Riddle", "I am Lord Voldemort") == True
	assert areAnagrams ("So dark the con of man", "Madonna of the Rocks") == True


if __name__ == "__main__":
	main()
