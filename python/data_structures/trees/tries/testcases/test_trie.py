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
	trie.add("abcd", "hello")
	trie.add("abc", "world")
	trie.add("acdc", "Back in Black")
	trie.add("bcdef", "bleargh!")

	assert(len(trie) == 5)
	assert(trie.prefixMatches("abc") == [("abc", "world"), ("abcd", "hello")])
	assert(trie.prefixMatches("a") == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black")])
	assert(trie.prefixMatches("") == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black"), ("bcdef", "bleargh!"), ("cdef", None)])
	assert(trie.prefixMatches() == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black"), ("bcdef", "bleargh!"), ("cdef", None)])
	assert(trie.prefixMatches("ad") == [])
	assert(trie.prefixMatches("b") == [("bcdef", "bleargh!")])
	assert(trie.prefixMatches("c") == [("cdef", None)])
	assert(trie.prefixMatches("d") == [])


def test_findKeysMatchingPrefix():
	trie = Trie()
	trie.add("cdef")
	trie.add("abcd")
	trie.add("abc")
	trie.add("acdc")
	trie.add("bcdef")

	assert(len(trie) == 5)
	assert(trie.findKeysMatchingPrefix("abc") == ["abc", "abcd"])
	assert(trie.findKeysMatchingPrefix("a") == ["abc", "abcd", "acdc"])
	assert(trie.findKeysMatchingPrefix("") == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.findKeysMatchingPrefix() == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.findKeysMatchingPrefix("ad") == [])
	assert(trie.findKeysMatchingPrefix("b") == ["bcdef"])
	assert(trie.findKeysMatchingPrefix("c") == ["cdef"])
	assert(trie.findKeysMatchingPrefix("d") == [])



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
	trie.add("abcd", "abcd")
	trie.add("abc", "testing abc")
	trie.add("abc", "testing abc")
	trie.add("abd", "Node abd")

	assert(trie.countPrefix("abc") == 2)
	assert(trie.countPrefix("a") == 3)
	assert(trie.frequency("abc") == 2)

	assert(len(trie) == 3)
	assert(trie.remove("") == None)
	assert(len(trie) == 3)
	assert(trie.remove("a") == None)
	assert(len(trie) == 3)
	assert(trie.hasWord("abc") == True)
	assert(trie.remove("abc", True) == "testing abc") # force remove
	assert(trie.hasWord("abc") == False)
	assert(len(trie) == 2)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 2)

	# already removed, now it only exists as a prefix
	assert(trie.remove("abc") == None)
	assert(len(trie) == 2)
	assert(trie.hasWord("abd") == True)
	assert(trie.remove("abd") == "Node abd")
	assert(trie.hasWord("abd") == False)
	assert(len(trie) == 1)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 1)

	assert(trie.hasWord("abcd") == True)
	assert(trie.remove("abcd") == "abcd")
	assert(len(trie) == 0)
	assert(trie.root == None)

	trie.add("abcd")
	trie.add("abcd")
	trie.add("abcd")
	assert(trie.frequency("abcd") == 3)
	assert(len(trie) == 1)
	assert(trie.hasPrefix("abc") == True)
	assert(trie.hasWord("abcd") == True)
	assert(trie.countPrefix("abc") == 1)
	assert(trie.countPrefix("a") == 1)

	assert(trie.remove("abcd") == None) # soft remove, reduce frequency
	assert(len(trie) == 1)
	assert(trie.hasWord("abcd") == True)
	assert(trie.frequency("abcd") == 2)

	trie.add("abcd", "what")
	assert(trie.frequency("abcd") == 3)
	
	trie.add("abc")

	assert(len(trie) == 2)
	assert(trie.remove("abcd", True) == "what") # remove unconditionally
	assert(len(trie) == 1)
	assert(trie.hasWord("abc") == True)
	assert(trie.hasWord("abcd") == False)

	assert(trie.remove("abc") == None) # soft remove, reduce frequency + delete because f:0
	assert(len(trie) == 0)
	assert(trie.root == None)

	trie.add("ab")
	trie.add("abcd")
	assert(len(trie) == 2)
	assert(trie.remove("abcd") == None)
	assert(len(trie) == 1)
	assert(trie.hasWord("ab") == True)
	assert(trie.hasWord("abcd") == False)
	assert(trie.hasPrefix("abc") == False)
	assert(trie.root['a'].children['b'].children.items == None)



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
	assert(trie.findKeysMatchingPrefix("a") == ["abd"])

	# try and remove 'abc' again - nothing should change
	trie.removePrefix("abc")  # removes 'abc', 'abcd' and 'abce'
	assert(len(trie) == 3)
	assert(trie.root['a'].children['b'].children.items.keys() == ['d'])
	assert(trie.countPrefix("abc") == 0)
	assert(trie.countPrefix("a") == 1)
	assert(trie.findKeysMatchingPrefix("a") == ["abd"])

	# remove 'a*'
	trie.removePrefix("a")  # removes 'abd'
	assert(len(trie) == 2)
	assert(sorted(trie.root.items.keys()) == ['s', 'w'])
	assert(trie.countPrefix("abc") == 0)
	assert(trie.countPrefix("a") == 0)
	assert(trie.findKeysMatchingPrefix("a") == [])
	assert(trie.findKeysMatchingPrefix("") == ["swords", "words"])

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

	try:
		# prefixes dont have storage
		assert(trie.search("swo") == None)
	except KeyError as e:
		assert(e.message == "__getitem__(): Word 'swo' not found in trie")




def test_indexing():
	trie = Trie()
	trie.add("abcd", "abcd")
	trie.add("abc", "ABC")
	trie.add("abd", "")
	trie.add("abce", (1,2))
	trie.add("words", ["one", "two", "three"])
	trie.add("swords")
	assert(len(trie) == 6)

	try:
		hit_exception = True
		assert(trie["a"] == 1)
		hit_exception = False
	except KeyError as e: # we haven't added 'a' yet, it exists only as a prefix
		assert(e.message == "__getitem__(): Word 'a' not found in trie")
	assert(hit_exception == True)

	trie["a"] = 1
	assert(trie.search("a") == 1)
	assert(trie["a"] == 1)
	assert(len(trie) == 7)

	assert(trie["swords"] == None)
	trie["swords"] = "new value"
	assert(trie["swords"] == "new value")
	assert(len(trie) == 7) # len doesn't change on update

	assert(trie["abcd"] == "abcd")
	assert(trie["abc"] == "ABC")
	assert(trie["abd"] == "")
	assert(trie["abce"] == (1,2))
	assert(trie["words"] == ["one", "two", "three"])



def test_destroy():
	trie = Trie()
	assert(trie.root == None)
	assert(len(trie) == 0)
	assert(not trie == True)

	trie.add("abcd", "abcd")
	trie.add("abc", "ABC")
	trie.add("abd", "")
	trie.add("abce", (1,2))
	trie.add("words", ["one", "two", "three"])
	trie.add("swords")
	assert(len(trie) == 6)

	trie.destroy()
	assert(trie.root == None)
	assert(len(trie) == 0)
	assert(not trie == True)



if __name__ == '__main__':
	test_add()
	test_hasWord()
	test_hasPrefix()
	test_frequency()
	test_countPrefix()
	test_findKeysMatchingPrefix()
	test_prefixMatches()
	test_sorted()
	test_remove()
	test_removePrefix()
	test_search()
	test_indexing()
	test_destroy()


	

