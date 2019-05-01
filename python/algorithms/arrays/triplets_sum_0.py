'''
Return all unique triplets that add upto 0
a + b + c = 0

Given array nums = [-1, 0, 1, 2, -1, -4],

A solution set is:
[
  [-1, 0, 1],
  [-1, -1, 2]
]
'''


# Find a pair that adds in 'array' that adds upto a given 'target_sum'
# O(n), single-pass
def find_pair(array, startidx, target_sum):
	# Create a reverse lookup map that returns index i, for array[i]
	lookup_map = {}
	pairs = []
	# Create reverse lookup as we match
	for i in range(startidx, len(array)):
		delta = target_sum - array[i]
		if lookup_map.has_key(delta):
			pairs.append((lookup_map[delta], i))

		lookup_map[array[i]] = i

	return pairs



# Add a triplet (a,b,c) to the triplets list if it doesn't already exist
# in the list (their relative order notwithstanding)
# (1, 0, -1) == (1, -1, 0) == (-1, 0, 1)
def add_unique_triplets(triplets, triplet):
	sorted_triplet = sorted(triplet)
	for t in triplets:
		# return without adding as soon as we find another triplet
		# with the same digits
		if sorted(t) == sorted_triplet:
			return

	# Entire list didn't have the same triplet (in whatever order)
	triplets.append(list(triplet))


# find triplets (a,b,c) in the array that add upto to 0
# a+b+c == 0, b+c == -a,
# Use find_pair(array, -a) to find a pair that adds upto -(b+c)
def find_triplet(array):
	triplets = []
	for i in range(len(array)):
		pairs = find_pair(array, i+1, -array[i])
		if pairs is not None:
			for (a,b) in pairs:
				add_unique_triplets(triplets, (array[i], array[a], array[b]))

	return triplets



if __name__ == '__main__':
	print find_triplet([1,2,3,-1,0])
	print find_triplet([-1, 0, 1, 2, -1, -4])

		


