'''
Disjoint-sets/Union-Find
  Use union by ranking, and path-compression optimizations to reduce the levels of lookup
'''
from node import Node
from collections import defaultdict

'''
Disjoint-set using union by rank and path-compression
'''
class DisjointSet(object):
	def __init__(self):
		# maps data to their respective nodes in the disjoint set
		# for quick lookup
		self.nodes_map = defaultdict(lambda: None)

	
	'''
	Make a singleton set containing 'value'
	'''
	def make_set(self, value):
		# A node already exists for 'value'
		if self.nodes_map[value] is not None:
			return
		self.nodes_map[value] = Node(value)



	'''
	Merge two sets containing lvalue, and rvalue respectively
	into one
	'''
	def union(self, lvalue, rvalue):
		lrep = self.find(lvalue)
		rrep = self.find(rvalue)

		if lrep is None or rrep is None:
			# One of the items doesn't have a matching set
			# return for now
			# TODO: Maybe consider making a set
			return

		# Both items have the same representative
		# and therefore are already in the same set
		if lrep == rrep:
			return

		if lrep.rank >= rrep.rank:
			# if left's representative has the same rank as the right
			# Move right under left.
			if lrep.rank == rrep.rank:
				lrep.rank += 1

			# Move right under left
			rrep.parent = lrep
		else:
			# Move left under right
			lrep.parent = rrep



	'''
	Find set containing a specified value
	'''
	def find(self, value):
		# Helper function to recursively find the representative
		# of the set 'value' belongs to
		# Readjust all parent links of Node(value) to the set representative
		# compressing their paths to the representative node
		def find_(node):
			if node.parent == None:
				return node
			node.parent = find_(node.parent)
			return node.parent

		node = None
		if isinstance(value, Node):
			# 'value' is already a wrapped node
			node = value
		else:
			# find node wrapping 'value'
			node = self.nodes_map[value]

		# Couldn't find node matching data
		if not node:
			return None

		return find_(node)



	'''
	Number of disjoint sets in the current structure
	'''
	def num_sets(self):
		n = 0
		for v in self.nodes_map.values():
			if v.parent is None:
				n += 1
		return n

			
	
if __name__ == '__main__':
	ds = DisjointSet()
	ds.make_set("apple")
	ds.make_set("orange")
	ds.make_set("grape")
	ds.make_set("pear")
	ds.make_set("spinach")
	ds.make_set("potato")
	ds.make_set("tomato")
	ds.make_set("lemon")
	ds.make_set("brinjal")

	fruits = ["apple", "orange", "grape", "pear", "lemon"]
	veggies = ["spinach", "potato", "tomato", "brinjal"]

	for f_v in fruits+veggies:
		n = ds.find(f_v)
		assert (n is not None)
		assert (n.key == f_v)

	# fruits set
	for i in xrange(len(fruits)-1):
		ds.union(fruits[i], fruits[i+1])

	for f in fruits:
		 assert (ds.find(f).key == fruits[0])

	# add two new fruits
	fruits.append("papaya")
	fruits.append("kiwi")

	ds.make_set(fruits[-1])
	ds.make_set(fruits[-2])
	ds.union(fruits[-1], fruits[-2])
	assert (ds.find(fruits[-1]).key == fruits[-1])
	assert (ds.find(fruits[-2]).key == fruits[-1])
	
	assert (ds.find(fruits[1]).rank == 1)
	assert (ds.find(fruits[-2]).rank == 1)
	assert (ds.find(fruits[-1]).rank == 1)

	# Merge the two fruits sets
	# 'kiwi' is the new representative
	ds.union(fruits[-2], fruits[1])

	assert (ds.nodes_map[fruits[0]].rank == 1)
	assert (ds.find(fruits[-1]).rank == 2)
	assert (ds.find(fruits[0]).rank == 2)
	assert (ds.find(fruits[-2]).key == fruits[-1]) 

	for f in fruits:
		 assert (ds.find(f).key == fruits[-1])

	# Veggies set
	for i in xrange(len(veggies)-1, 0, -1):
		ds.union(veggies[i], veggies[i-1])

	for i in xrange(len(veggies)-1):
		# all of these are already in the same set
		# represented by 'brinjal'
		ds.union(veggies[i], veggies[i+1])

	for v in veggies:
		assert (ds.find(v).key == veggies[-1])

	'''
	for x,v in ds.nodes_map.items():
		print x,repr(v)
	'''

	assert(ds.num_sets() == 2)


