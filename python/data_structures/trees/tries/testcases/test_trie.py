import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Trie, Node, TrieEmptyError


# trie node testcases
def node_testcases():
	n = Node()
	assert(len(n.children) == 0)

	n.add('A')
	assert(len(n.children) == 1)

	n.end_of_word = True
	assert(len(n.children) == 1)
	assert(n.end_of_word == True)
	n.children['A'] = 0x1234 # just a test address

	n.add('A') # this should do nothing
	assert(len(n.children) == 1)
	assert(n.end_of_word == True)
	assert(n.children['A'] == 0x1234)
	assert(n.getChildNode('A') == 0x1234)

	n.add('B')
	assert(len(n.children) == 2)
	assert(sorted(n.children.keys()) == ['A', 'B'])
	assert(not n.getChildNode('B') == True) # empty child node
	assert(n.getChildNode('B').frequency == 0) # empty child node
	assert(n.getChildNode('B').end_of_word == False) # empty child node

	n.children.pop('A') # create a new node and make it the child node of 'A' at root
	n.add('A')
	assert(n.getChildNode('A') != 0x1234)
	nc = n.getChildNode('A')
	nc.add('B')
	nc.frequency = 1
	nc.end_of_word = True # this means 'A' is a word
	assert(nc.end_of_word == True)
	assert(nc.frequency == 1)

	assert(str(nc) == "[1]: ['B']")
	nc.remove('B')
	assert(not nc == True)
	assert(len(nc.children) == 0)

	# 'A" is not set in nc
	# nothing needs to be done
	nc.remove('A')
	assert(not nc == True)
	assert(len(nc.children) == 0)

	assert(str(n) == "[2]: ['A', 'B']")
	assert(str(nc) == "[0]: []")

	nc.add('C') # AC
	assert(str(nc) == "[1]: ['C']")
	nc.getChildNode('C').end_of_word = True
	assert(n.getChildNode('A').getChildNode('C').end_of_word == True)

	# removing 'A' should not do anything at this point,
	# since there's AC on the trie
	n.remove('A')
	assert(str(n) == "[1]: ['B']")

	# try to remove 'C' from child node
	nc.remove('C')
	assert(len(nc.children) == 0) # 'C' was removed


# trie node testcases

# trie testcases

'''
Test add word to trie (iterative version)
'''
def test_add(recursive=False):
	trie = Trie()
	assert(trie.num_words == 0)

	add_fn = trie.add
	if recursive:
		add_fn = trie.add_r

	add_fn("abcd")
	assert(trie.num_words == 1)
	assert(trie.root.getChildNode('a').end_of_word == False)
	assert(trie.root.getChildNode('a').frequency == 0)
	assert(trie.root.getChildNode('a').getChildNode('b').end_of_word == False)
	assert(trie.root.getChildNode('a').getChildNode('b').frequency == 0)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').end_of_word == False)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').frequency == 0)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').getChildNode('d').end_of_word == True)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').getChildNode('d').frequency == 1)

	add_fn("abc", "first word")
	assert(trie.num_words == 2)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').end_of_word == True)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').frequency == 1)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').getChildNode('d').end_of_word == True)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').data == "first word")
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').getChildNode('d').frequency == 1)

	add_fn("abc") # Increase frequency
	assert(trie.num_words == 2)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').data == None)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').end_of_word == True)
	assert(trie.root.getChildNode('a').getChildNode('b').getChildNode('c').frequency == 2)


def test_hasPrefix(recursive=False):
	trie = Trie()
	#assert(trie.hasPrefix("word") == False)

	trie.add("word")
	assert(trie.root.getChildNode('w').getChildNode('o').getChildNode('r').getChildNode('d').end_of_word == True)

	trie.add_r("words")
	trie.add("sword")

	assert(trie.root.getChildNode('w').getChildNode('o').getChildNode('r').getChildNode('d').end_of_word == True)
	assert(trie.root.getChildNode('w').getChildNode('o').getChildNode('r').getChildNode('d').getChildNode('s').end_of_word == True)

	assert(len(trie) == 3)

	hasPrefix_fn = trie.hasPrefix
	if recursive:
		hasPrefix_fn = trie.hasPrefix_r

	assert(hasPrefix_fn("word") == True)
	assert(hasPrefix_fn("wor") == True)
	assert(hasPrefix_fn("wort") == False)
	assert(hasPrefix_fn("words") == True)
	assert(hasPrefix_fn("sword") == True)
	assert(hasPrefix_fn("swo") == True)
	assert(hasPrefix_fn("so") == False)



def test_hasWord(recursive=False):
	trie = Trie()
	assert(trie.hasWord("word") == False)

	trie.add("word")
	trie.add("words")
	trie.add_r("sword")
	assert(len(trie) == 3)

	hasWord_fn = trie.hasWord
	if recursive:
		hasWord_fn = trie.hasWord_r

	assert(hasWord_fn("word") == True)
	assert(hasWord_fn("wort") == False)
	assert(hasWord_fn("words") == True)
	assert(hasWord_fn("wor") == False)
	assert(hasWord_fn("sword") == True)
	assert(hasWord_fn("swords") == False)


def test_frequency(recursive=False):
	trie = Trie()
	assert(trie.frequency("word") == 0)

	trie.add("word")
	trie.add("word")
	trie.add_r("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")

	frequency = trie.frequency
	if recursive:
		frequency = trie.frequency_r

	assert(len(trie) == 3)
	assert(frequency("word") == 2)
	assert(frequency("wor") == 0)
	assert(frequency("wort") == 0)
	assert(frequency("words") == 4)
	assert(frequency("sword") == 1)
	assert(frequency("swo") == 0)
	assert(frequency("so") == 0)



'''
Test [] for get and set
'''
def test_indexing():
	trie = Trie()
	trie.add("abcd", "abcd")
	trie.add_r("abc", "ABC")
	trie.add("abd", "")
	trie.add_r("abce", (1,2))
	trie.add("words", ["one", "two", "three"])
	trie.add_r("swords")
	assert(len(trie) == 6)

	try:
		hit_exception = True
		assert(trie["a"] == 1)
		hit_exception = False
	except KeyError as e: # we haven't added 'a' yet, it exists only as a prefix
		assert(e.message == "__getitem__(): Word 'a' not found in trie")
	assert(hit_exception == True)

	trie["a"] = 1
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

	dictionary = Trie()
	definition = "adjective [hi-stawr-ee-ey-tid, -stohr-] decorated with animals, flowers, or other designs that have a narrative or symbolic purpose, especially of initial letters on an illuminated manuscript."
	dictionary.add("historiated", definition)
	assert(dictionary["historiated"] == definition)
	dictionary.add("Zephyr", "light wind")
	assert(dictionary["Zephyr"] == "light wind")

	dictionary["historiated"] = "decorated"
	assert(dictionary["historiated"] == "decorated")

	# Add word using [] syntax
	dictionary["frozen"] = "cold"
	assert(dictionary["frozen"] == "cold")
	assert(len(dictionary) == 3)




def test_dfs():
	trie = Trie()
	trie.add("a", "1")
	trie.add("b", "2")
	trie.add("ab", "12")
	trie.add("bc", "23")
	trie.add("abc", "123")
	trie.add("bde", "245")

	def fn(a, b, l):
		l.append((a, b))

	l = []
	trie.dfs("a", fn, l)
	assert(l == [('a', '1'), ('ab', '12'), ('abc', '123')])

	l = []
	trie.dfs("", fn, l)
	assert l == [('a', '1'), ('ab', '12'), ('abc', '123'), ('b', '2'), ('bc', '23'), ('bde', '245')]

	l = []
	trie.dfs("b", fn, l)
	assert(l == [('b', '2'), ('bc', '23'), ('bde', '245')])

	def counter(a, b, l):
		if not l:
			l.append(0)
		l[0] += 1

	l = []
	trie.dfs("a", counter, l)
	assert(l[0] == 3)

	l = []
	trie.dfs("", counter, l)
	assert(l[0] == len(trie))

	l = []
	trie.dfs("b", counter, l)
	assert(l[0] == 3)

	l = []
	trie.dfs("bde", counter, l)
	assert(l[0] == 1)


def test_findWords():
	trie = Trie()
	trie.add("cdef")
	trie.add("abcd")
	trie.add("abc")
	trie.add("acdc")
	trie.add("bcdef")

	assert(len(trie) == 5)
	assert(trie.findWords("abc") == ["abc", "abcd"])
	assert(trie.findWords("a") == ["abc", "abcd", "acdc"])
	assert(trie.findWords("") == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.findWords() == ["abc", "abcd", "acdc", "bcdef", "cdef"])
	assert(trie.findWords("ad") == [])
	assert(trie.findWords("b") == ["bcdef"])
	assert(trie.findWords("c") == ["cdef"])
	assert(trie.findWords("d") == [])


def test_search():
	trie = Trie()
	trie.add("cdef")
	trie.add("abcd", "hello")
	trie.add("abc", "world")
	trie.add("acdc", "Back in Black")
	trie.add("bcdef", "bleargh!")

	assert(len(trie) == 5)
	assert(trie.search("abc") == [("abc", "world"), ("abcd", "hello")])
	assert(trie.search("a") == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black")])
	assert(trie.search("") == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black"), ("bcdef", "bleargh!"), ("cdef", None)])
	assert(trie.search() == [("abc", "world"), ("abcd", "hello"), ("acdc", "Back in Black"), ("bcdef", "bleargh!"), ("cdef", None)])
	assert(trie.search("ad") == [])
	assert(trie.search("b") == [("bcdef", "bleargh!")])
	assert(trie.search("c") == [("cdef", None)])
	assert(trie.search("d") == [])


def test_count():
	t = Trie()
	t.add("abcd")
	assert(len(t) == 1)
	assert(t.count("abc") == 1)

	trie = Trie()
	hit_exception = False
	try:
		assert(trie.count("word") == 0)
	except TrieEmptyError as e:
		assert(e.message == "TrieEmptyError: 'count(): Trie is empty'")
		hit_exception = True

	assert(hit_exception == True)	

	trie.add("word")
	trie.add("word")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("words")
	trie.add("sword")
	trie.add("ward")

	assert(len(trie) == 4)
	assert(len(trie) == trie.count(""))
	assert(trie.count("w") == 3)
	assert(trie.count("word") == 2)
	assert(trie.count("wor") == 2)
	assert(trie.count("wort") == 0)
	assert(trie.count("words") == 1)
	assert(trie.count("sword") == 1)
	assert(trie.count("swo") == 1)
	assert(trie.count("so") == 0)


def test_remove():
	trie = Trie()
	trie.add("abcd", "abcd")
	trie.add("abc", "testing")
	trie.add("abc", "testing abc")
	trie.add("abd", "Node abd")

	assert(trie.count("abc") == 2)
	assert(trie.count("a") == 3)
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
	assert(trie.count("abc") == 1)
	assert(trie.count("a") == 2)

	# already removed, now it only exists as a prefix
	assert(trie.remove("abc") == None)
	assert(len(trie) == 2)
	assert(trie.hasWord("abd") == True)
	assert(trie.remove("abd") == "Node abd")
	assert(trie.hasWord("abd") == False)
	assert(len(trie) == 1)
	assert(trie.count("abc") == 1)
	assert(trie.count("a") == 1)

	assert(trie.hasWord("abcd") == True)
	assert(trie.remove("abcd") == "abcd")
	assert(len(trie) == 0)
	assert(trie.root is not None)

	trie.add("abcd")
	trie.add("abcd")
	trie.add("abcd")
	assert(trie.frequency("abcd") == 3)
	assert(len(trie) == 1)
	assert(trie.hasPrefix("abc") == True)
	assert(trie.hasWord("abcd") == True)
	assert(trie.count("abc") == 1)
	assert(trie.count("a") == 1)

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
	assert(trie.root is not None)

	trie.add("ab")
	trie.add("abcd")
	assert(len(trie) == 2)
	assert(trie.remove("abcd") == None)
	assert(len(trie) == 1)
	assert(trie.hasWord("ab") == True)
	assert(trie.hasWord("abcd") == False)
	assert(trie.hasPrefix("abc") == False)


def test_removePrefix():
	trie = Trie()
	trie.add("abcd")
	trie.add("abc")
	trie.add("abd")
	trie.add("abce")
	trie.add("words")
	trie.add("swords")

	assert(trie.count("abc") == 3)
	assert(trie.count("a") == 4)

	assert(len(trie) == 6)
	trie.removePrefix("abc")  # removes 'abc', 'abcd' and 'abce'

	# 'ab' should only have 'd' as its child now
	assert(len(trie) == 3)
	assert(trie.count("abc") == 0)
	assert(trie.count("a") == 1)
	assert(trie.findWords("a") == ["abd"])

	# try and remove 'abc' again - nothing should change
	trie.removePrefix("abc")  # NO-OP
	assert(len(trie) == 3)
	assert(trie.count("abc") == 0)
	assert(trie.count("a") == 1)
	assert(trie.findWords("a") == ["abd"])

	# remove 'a*'
	trie.removePrefix("a")  # removes 'abd'
	assert(len(trie) == 2)
	assert(trie.count("abc") == 0)
	assert(trie.count("a") == 0)
	assert(trie.findWords("a") == [])
	assert(trie.findWords("") == ["swords", "words"])

	# flush the whole trie
	trie.removePrefix("")
	assert(trie.root is not None)
	assert(len(trie) == 0)

	trie.add("words")
	trie.add("newly")
	trie.add("added")
	assert(len(trie) == 3)

	# flush the whole trie again
	trie.removePrefix("")
	assert(trie.root is not None)
	assert(len(trie) == 0)


def test_destroy():
	trie = Trie()
	assert(trie.root is not None)
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
	assert(trie.root is not None)
	assert(len(trie) == 0)
	assert(not trie == True)

	trie.add("new", "new set")
	trie.add("words", "from dictionary")
	assert(len(trie) == 2)
	assert(trie["new"] == "new set")
	assert(trie["words"] == "from dictionary")
	trie.destroy()
	assert(trie.root is not None)
	assert(len(trie) == 0)
	assert(not trie == True)



def trie_testcases():
	test_add()
	test_add(recursive=True)
	test_hasPrefix()
	test_hasPrefix(recursive=True)
	test_hasWord()
	test_hasWord(recursive=True)
	test_frequency()
	test_frequency(recursive=True)
	test_indexing()
	test_dfs()
	test_findWords()
	test_search()
	test_count()
	test_remove()
	test_removePrefix()
	test_destroy()


# trie testcases




if __name__ == '__main__':
	node_testcases()
	trie_testcases()

