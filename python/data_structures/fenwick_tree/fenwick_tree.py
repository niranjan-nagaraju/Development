
# Calculate cumulative frequencies for a table of frequencies
def cumulative_frequency (f):
	cf = []
	sum = 0
	for l in f:
		sum += l
		cf.append(sum)

	return cf


# Read BIT/fenwick tree for given event idx == cumulative_frequency (idx)
def read_bit(tree, idx):
	sum = 0
	while idx > 0:
		sum += tree[idx]
		idx -= (idx & -idx) 

	return sum

# Update BIT for event at idx
def update_bit(tree, idx, val):
	while (idx < len(tree)):
		tree[idx] += val;
		idx += (idx & -idx)


# Construct BIT/fenwick_tree from a frequency table 
def construct_bit (f):
	tmp_tree = [0] * len(f)
	for i in range(1, len(f)):
		update_bit(tmp_tree, i, f[i])

	return tmp_tree
	

if __name__ == "__main__":
    f = [0, 1, 0, 2, 1, 1, 3, 0, 4, 2, 5, 2, 2, 3, 1, 0, 2]

    cf = cumulative_frequency(f)
    tree = construct_bit(f) # [0, 1, 1, 2, 4, 1, 4, 0, 12, 2, 7, 2, 11, 3, 4, 0, 29]

    if read_bit (tree, 13) == cf[13] == 26:
        print 'CF Tests success'
