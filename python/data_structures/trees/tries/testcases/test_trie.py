import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Trie, TrieEmptyError



def test_add():
	trie = Trie()

	assert(len(trie) == 0)
	assert(not trie == True)
	trie.add("word")
	assert(len(trie) == 1)

	node = trie.root
	assert(not node['w'] == False)
	assert(len(node) == 1) # only 1 character 'w' set in root
	assert(repr(node) == "[1]: ('w', $:False f:0)")

	node = node['w'].children
	assert(not node == False)
	assert(not node['o'] == False)
	assert(len(node) == 1) # only 1 character 'o' set in level-1
	assert(repr(node) == "[1]: ('o', $:False f:0)")

	node = node['o'].children
	assert(not node == False)
	assert(not node['r'] == False)
	assert(len(node) == 1) # only 1 character 'r' set in level-2
	assert(repr(node) == "[1]: ('r', $:False f:0)")

	node = node['r'].children
	assert(not node == False)
	assert(not node['d'] == False)
	assert(len(node) == 1) # only 1 character 'd' set in level-3
	assert(repr(node) == "[1]: ('d', $:True f:1)")

	# Add a second word, "words"
	trie.add("words")
	assert(len(trie) == 2)

	node = trie.root
	assert(not node['w'] == False)
	assert(len(node) == 1) # only 1 character 'w' set in root
	assert(repr(node) == "[1]: ('w', $:False f:0)")

	node = node['w'].children
	assert(not node == False)
	assert(not node['o'] == False)
	assert(len(node) == 1) # only 1 character 'o' set in level-1
	assert(repr(node) == "[1]: ('o', $:False f:0)")

	node = node['o'].children
	assert(not node == False)
	assert(not node['r'] == False)
	assert(len(node) == 1) # only 1 character 'r' set in level-2
	assert(repr(node) == "[1]: ('r', $:False f:0)")

	node = node['r'].children
	assert(not node == False)
	assert(not node['d'] == False)
	assert(len(node) == 1) # only 1 character 'd' set in level-3
	assert(repr(node) == "[1]: ('d', $:True f:1)")

	node = node['d'].children
	assert(not node == False)
	assert(not node['s'] == False)
	assert(len(node) == 1) # only 1 character 's' set in level-4
	assert(repr(node) == "[1]: ('s', $:True f:1)")

	# Add a third word, "abc"
	trie.add("abc")
	assert(len(trie) == 3)

	node = trie.root
	assert(not node['a'] == False)
	assert(len(node) == 2) # only 2 characters 'w' and 'a' set in root
	assert(repr(node) == "[2]: ('a', $:False f:0) ('w', $:False f:0)")

	node = node['a'].children
	assert(not node == False)
	assert(not node['b'] == False)
	assert(len(node) == 1) # only 1 character 'b' set in level-1 below 'a'
	assert(repr(node) == "[1]: ('b', $:False f:0)")

	node = node['b'].children
	assert(not node == False)
	assert(not node['c'] == False)
	assert(len(node) == 1) # only 1 character 'c' set in level-2 below 'b'
	assert(repr(node) == "[1]: ('c', $:True f:1)")






def test_hasWord():
	trie = Trie()
	try:
		assert(trie.hasWord("word") == False)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'hasWord(): Trie is empty'")

	trie.add("word")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.hasWord("word") == True)
	assert(trie.hasWord("wort") == False)
	assert(trie.hasWord("words") == True)
	assert(trie.hasWord("wor") == False)
	assert(trie.hasWord("sword") == True)
	assert(trie.hasWord("swords") == False)


def test_hasPrefix():
	trie = Trie()
	try:
		assert(trie.hasPrefix("word") == False)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'hasPrefix(): Trie is empty'")

	trie.add("word")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.hasPrefix("word") == True)
	assert(trie.hasPrefix("wor") == True)
	assert(trie.hasPrefix("wort") == False)
	assert(trie.hasPrefix("words") == True)
	assert(trie.hasPrefix("sword") == True)
	assert(trie.hasPrefix("swo") == True)
	assert(trie.hasPrefix("so") == False)



def test_frequency():
	trie = Trie()
	try:
		assert(trie.frequency("word") == 0)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'frequency(): Trie is empty'")

	trie.add("word")
	trie.add("word")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")

	assert(len(trie) == 3)
	assert(trie.frequency("word") == 2)
	assert(trie.frequency("wor") == 0)
	assert(trie.frequency("wort") == 0)
	assert(trie.frequency("words") == 4)
	assert(trie.frequency("sword") == 1)
	assert(trie.frequency("swo") == 0)
	assert(trie.frequency("so") == 0)


def test_countPrefix():
	t = Trie()
	t.add("abcd")
	#t.add("abc")
	assert(len(t) == 1)
	assert(t.countPrefix("abc") == 1)

	trie = Trie()
	try:
		assert(trie.countPrefix("word") == 0)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'countPrefix(): Trie is empty'")

	trie.add("word")
	trie.add("word")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")
	trie.add("ward")

	assert(len(trie) == 4)
	assert(trie.countPrefix("w") == 3)
	assert(trie.countPrefix("word") == 2)
	assert(trie.countPrefix("wor") == 2)
	assert(trie.countPrefix("wort") == 0)
	assert(trie.countPrefix("words") == 1)
	assert(trie.countPrefix("sword") == 1)
	assert(trie.countPrefix("swo") == 1)
	assert(trie.countPrefix("so") == 0)



def test_prefixMatches():
	trie = Trie()
	trie.add("cdef")
	trie.add("abcd")
	trie.add("abc")
	trie.add("acdc")
	trie.add("bcdef")

	assert(len(trie) == 5)
	assert(trie.prefixMatches("abc") == ["abc", "abcd"])
	assert(trie.prefixMatches("a") == ["abc", "abcd", "acdc"])
	assert(trie.prefixMatches("") == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.prefixMatches() == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.prefixMatches("ad") == [])
	assert(trie.prefixMatches("b") == ["bcdef"])
	assert(trie.prefixMatches("c") == ["cdef"])
	assert(trie.prefixMatches("d") == [])



def test_sorted():
	trie = Trie()
	trie.add("words")
	trie.add("swords")
	trie.add("cdef")
	trie.add("abcd")
	trie.add("abc")
	trie.add("acdc")
	trie.add("bcdef")
	trie.add("bakery")
	trie.add("oven")
	trie.add("supercalifragilisticexpialidocious")

	assert(len(trie) == 10)
	assert(trie.sorted() == ['abc', 'abcd', 'acdc', 'bakery', 'bcdef', 'cdef', 'oven', 
		'supercalifragilisticexpialidocious', 'swords', 'words'])



def test_remove():
	trie = Trie()
	trie.add("abcd")
	trie.add("abc")
	trie.add("abd")

	assert(trie.countPrefix("abc") == 2)
	assert(trie.countPrefix("a") == 3)

	assert(len(trie) == 3)
	assert(trie.remove("") == False)
	assert(len(trie) == 3)
	assert(trie.remove("a") == False)
	assert(len(trie) == 3)
	assert(trie.hasWord("abc") == True)
	assert(trie.remove("abc") == True)
	assert(trie.hasWord("abc") == False)
	assert(len(trie) == 2)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 2)

	# already removed, now it only exists as a prefix
	assert(trie.remove("abc") == False)
	assert(len(trie) == 2)
	assert(trie.hasWord("abd") == True)
	assert(trie.remove("abd") == True)
	assert(trie.hasWord("abd") == False)
	assert(len(trie) == 1)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 1)

	assert(trie.hasWord("abcd") == True)
	assert(trie.remove("abcd") == True)
	assert(len(trie) == 0)
	assert(trie.root == None)

	trie.add("abcd")
	assert(len(trie) == 1)
	assert(trie.hasPrefix("abc") == True)
	assert(trie.hasWord("abcd") == True)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 1)

	assert(trie.remove("abcd") == True)
	assert(len(trie) == 0)
	assert(trie.root == None)


def test_removePrefix():
	trie = Trie()
	trie.add("abcd")
	trie.add("abc")
	trie.add("abd")
	trie.add("abce")
	trie.add("words")
	trie.add("swords")

	assert(trie.countPrefix("abc") == 3)
	assert(trie.countPrefix("a") == 4)

	assert(len(trie) == 6)
	trie.removePrefix("abc")  # removes 'abc', 'abcd' and 'abce'

	# 'ab' should only have 'd' as its child now
	assert(len(trie) == 3)
	assert(trie.root['a'].children['b'].children.items.keys() == ['d'])
	assert(trie.countPrefix("abc") == 0)
	assert(trie.countPrefix("a") == 1)
	assert(trie.prefixMatches("a") == ["abd"])

	# try and remove 'abc' again - nothing should change
	trie.removePrefix("abc")  # removes 'abc', 'abcd' and 'abce'
	assert(len(trie) == 3)
	assert(trie.root['a'].children['b'].children.items.keys() == ['d'])
	assert(trie.countPrefix("abc") == 0)
	assert(trie.countPrefix("a") == 1)
	assert(trie.prefixMatches("a") == ["abd"])

	# remove 'a*'
	trie.removePrefix("a")  # removes 'abd'
	assert(len(trie) == 2)
	assert(sorted(trie.root.items.keys()) == ['s', 'w'])
	assert(trie.countPrefix("abc") == 0)
	assert(trie.countPrefix("a") == 0)
	assert(trie.prefixMatches("a") == [])
	assert(trie.prefixMatches("") == ["swords", "words"])

	# flush the whole trie
	trie.removePrefix("")
	assert(trie.root == None)
	assert(len(trie) == 0)



def test_search():
	trie = Trie()
	try:
		assert(trie.search("word") == None)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'search(): Trie is empty'")

	trie.add("word", "word #1")
	trie.add("words", "plural of words")
	trie.add("sword", "sword sheath excalibur")

	assert(len(trie) == 3)
	assert(trie.search("word") == "word #1")
	assert(trie.frequency("word") == 1)

	trie.add("word")
	assert(trie.search("word") == None) # overrridden by second 'add'
	assert(trie.frequency("word") == 2)

	trie.add("word", "words are found in a dictionary")
	assert(trie.search("word") == "words are found in a dictionary") # overrridden by third 'add'
	assert(trie.frequency("word") == 3)

	assert(trie.search("words") == "plural of words")
	assert(trie.search("sword") == "sword sheath excalibur")

	# prefixes dont have storage
	assert(trie.search("swo") == None)



if __name__ == '__main__':
	test_add()
	test_hasWord()
	test_hasPrefix()
	test_frequency()
	test_countPrefix()
	test_prefixMatches()
	test_sorted()
	test_remove()
	test_removePrefix()
	test_search()

	

