'''
Generate all permutations of a string

Solution: (CTCI)
Start with {a}
Add b at all slots: {ab} {ba}
Add c at all slots: {cab} {acb} {abc} {cba} {bca} {bac}

--------------------------------
Start with {abc}
permute (abc) -> permute(bc) -> permute(c) -> ""
                                      -> [c]
                 -> {bc, cb}									  
	-> {abc, bac, bca, acb, cab, cba} 
'''	

def permute(string):
	perms = []

	if string == None:
		return None
	elif len(string) == 0:
		perms.append("")
		return perms

	words = permute(string[1:])
	print 'Words:', words, perms
	for word in words:
		for j in range(len(word)+1):
			perms.append(word[:j] + string[0] + word[j:])

	return perms


print permute("abc")

'''
Trial run:
Words: ['']
Words: ['c']
Words: ['bc', 'cb']
['abc', 'bac', 'bca', 'acb', 'cab', 'cba']
'''
