import sys
sys.path.append("../../../../")
from data_structures.trees.tries.trie import Trie


def node_testcases():
	ni = Trie.NodeItem()
	ni2 = Trie.NodeItem()
	assert ni == ni2
	ni2.eow = True
	assert ni != ni2

	n = Trie.Node()
	assert(len(n.keys.keys()) == 0)
	n['a'] = Trie.NodeItem('words with a')
	assert len(n.keys.keys()) == 1
	assert n['a'].data == 'words with a'
	assert n['a'].eow == False
	assert n['a'].child == None

	n2 = Trie.Node()
	n2['b'] = Trie.NodeItem('ab', eow=True)
	assert len(n2.keys.keys()) == 1
	assert n2['b'].data == 'ab'
	assert n2['b'].eow == True
	assert n2['b'].child == None

def trie_testcases():
	t = Trie()
	assert t.root == Trie.Node()

	t.add('ab', 'abadidea')
	n1 = t.root
	assert len(n1.keys) == 1
	assert n1.keys.keys() == ['a']
	
	n2 = n1['a'].child
	assert n2
	assert n2.keys.keys() == ['b']
	assert n2['b'] == Trie.NodeItem( 'abadidea', True )
	assert n2['b'].child == None
	assert n2['b'].eow == False
	assert n2['b'].child == None

if __name__ == '__main__':
	node_testcases()
	trie_testcases()
