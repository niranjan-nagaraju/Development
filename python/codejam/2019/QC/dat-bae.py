'''
https://codingcompetitions.withgoogle.com/codejam/round/0000000000051705/000000000008830b

n,b,f:
	n: number of bits / workers
	b: number of faulty workers
	f: max number of tries to deduce faulty workers

write 'n' bits, read back (n-b) bits
'''



'''
=====================
Solution description:
=====================

***** start with assuming no 'f' and unlimited guesses are available
faults = []
iter 1: 
  w: 0111.....
  r: count number of 0s, if num0 != iter + len(faults), add iter to faults, 
iter 2:
   w:001111....
   r: count number of 0s, if num0 != iter + len(faults), add iter to faults,
   . . .
iter n: 00000..0 
   r: count number of 0s, if num0 != iter + len(faults), add iter to faults,

if at any given iter, len(faults) == b
we are done.
but worst case scenario, we might have to do 'n' writes

e.g.
faults = []
n=5, b=2, {faults at indices 1, 3}
iter 1:
     w: 01111
     r: 011
     no fault at bit 0

iter 2:
    w: 00111
     r:  011 (num 0s < 2),
     faults = [1]

iter 3:
   w: 00011
   r:  001 (num 0s + len(faults) == 3) => no faults

iter 4:
   w: 00001
    r:  001 (num 0s + len(faults) < 4) => faults = [1,3]
   b == len(faults)


***** This requires 'N' writes in the worst case, but also yields well to parallel processing.
Divide 'N' bits into groups of 'f' bits each, solve each group parallely.
since, each group has 'f' bits, number of faulty bits in each group can be guessed using atmost 'f' guesses.

Initially send,
(say if n=20, f=5) => 4 groups
00000 11111 00000 11111
to deduce number of faults in each group, and consequently what portion of the read bits correspond to which group.

Since, this write counts as 1 write, we are left with (f-1) guesses.
=>
Create groups with (f-1) bits in each group.

if n s not divisible by f, e.g, (n=25, f=10),
then solve as usual but with the last group containing just 5 bits.
so it becomes f(10, start=0, end=9) + f(10, start=10, end=19) + f(5, start=20, end=24)
'''

# Given a list of (n-b) 'read' bits, corresponding to a group of n bits [start .. end]
# at iteration, i, where we have written i+1 leading 0s
#  => iteration, (i+1) == len(faults) + 0s in the read: current bit 'start + i' is not faulty.
#  =>    else: 'start+i' is faulty, record it in faults list and return.
# figure out if the current read reveals a faulty bit, if so, 
# add the faulty worker/bit to the faults list
def find_faulty_worker(read_sequence, start, end, i, faults):
	# start index crosses over to the right of the 'n' bits list
	# => this group is empty
	if start > end:
		return

	# iteration, i, we have written (i+1) leading 0s
	num0s = read_sequence.count(0)

	if (i+1) != (num0s + len(faults)):
		# found a faulty worker
		# we are only dealing with 1 faulty worker at a time,
		# add this worker to faulty list and return
		faults.append(start + i)
		return



# divide n into 'x' groups with 'f-1' bits in each
def divide_groups(n, f):
	#f = f -1
	ngroups = n/f + 1
	for i in xrange(ngroups-1):
		print 'start, end:', i*f, i*f+f-1
	
	print (i+1)*f, n-1
	return [[] for _ in range(ngroups)]



# Test driver for write()
def write(sequence):
	write.write_buffer = sequence


# Test driver for read()
# read previously "written" 'sequence' into memory buffer,
# skip indices specified in the faulty_bits_idxs
def read(faulty_bits_idxs=[]):
	memory_buffer = []
	for i in xrange(len(write.write_buffer)):
		# skip faulty workers
		if i in faulty_bits_idxs:
			continue

		memory_buffer.append(write.write_buffer[i])

	return memory_buffer


# send alternate blocks of 0s and 1s to determine a breakup of
# faulty and valid workers in each group
def groupwise_splits(ngroups, n, f, faulty_bits_idxs=[]):
	sequence_1s = [1] * f
	sequence_0s = [0] * f
	sequence = []
	curr_sequence = sequence_0s
	for _ in xrange(ngroups-1):
		sequence += curr_sequence
		curr_sequence = sequence_1s if curr_sequence == sequence_0s else sequence_0s

	# Last group either contains no bits (n is divisible by f), or
	# contains fewer than f bits
	sequence += curr_sequence[:(n%f)]
	print len(sequence), sequence

	write(sequence)
	read_sequence = read(faulty_bits_idxs)
	return read_sequence




def test_find_faulty_worker():
	faults = []
	# workers {1,3} are faulty, but this is not known to the algorithm yet
	# iter 0, n=5, b=2, sent 01111, read back 011 => worker 0 is NOT faulty
	# w: 01111
	# r: 0 1 1
	find_faulty_worker([0,1,1], 0, 4, 0, faults)
	assert faults == []

	# iter 1, n=5, b=2, sent 00111, read back 011 => worker 1 is faulty
	# w: 00111
	# r: 0 1 1
	find_faulty_worker([0,1,1], 0, 4, 1, faults)
	assert faults == [1]

	# iter 2, n=5, b=2, sent 00011, read back 001 => worker 2 is NOT faulty
	# w: 00011
	# r: 0 0 1
	find_faulty_worker([0,0,1], 0, 4, 2, faults)
	assert faults == [1]

	# iter 3, n=5, b=2, sent 00001, read back 001 => worker 3 is faulty
	# w: 00001
	# r: 0 0 1
	find_faulty_worker([0,0,1], 0, 4, 3, faults)
	assert faults == [1, 3]

	# iter 4, n=5, b=2, sent 00000, read back 000 => worker 4 is NOT faulty
	# w: 00000
	# r: 0 0 0
	find_faulty_worker([0,0,1], 0, 4, 3, faults)
	assert faults == [1, 3]

	'''
	TC 2
	'''
	# 3 groups of 5 bits each
	# bit #1, #3 is broken in group1, #2, #4 in group2, #0 in group 3 - unbeknownst to the algorithm :)
	faults = [[] for _ in xrange(3)]
	# iter 0,
	# w: 01111 01111 01111
	# r: 0 1 1 01 1   1111
	find_faulty_worker([0,1,1],   0,  4,  0, faults[0])
	find_faulty_worker([0,1,1],   5,  9,  0, faults[1])
	find_faulty_worker([1,1,1,1], 10, 14, 0, faults[2])
	assert faults == [[],[],[10]]

	# iter 1,
	# w: 00111 00111 00111
	# r: 0 1 1 00 1   0111
	find_faulty_worker([0,1,1],   0,  4,  1, faults[0])
	find_faulty_worker([0,0,1],   5,  9,  1, faults[1])
	find_faulty_worker([0,1,1,1], 10, 14, 1, faults[2])
	assert faults == [[1],[],[10]]

	# iter 2,
	# w: 00011 00011 00011
	# r: 0 0 1 00 1   0011
	find_faulty_worker([0,0,1],   0,  4,  2, faults[0])
	find_faulty_worker([0,0,1],   5,  9,  2, faults[1])
	find_faulty_worker([0,0,1,1], 10, 14, 2, faults[2])
	assert faults == [[1],[7],[10]]
	return

	# iter 3,
	# w: 00001 00001 00001
	# r: 0 0 1 00 1   0001
	find_faulty_worker([0,0,1],   0,  4,  3, faults[0])
	find_faulty_worker([0,0,1],   5,  9,  3, faults[1])
	find_faulty_worker([1,1,1,1], 10, 14, 3, faults[2])
	assert faults == [[1,3],[7],[10]]

	# iter 4,
	# w: 00000 00000 00000
	# r: 0 0 0 00 0   0000
	find_faulty_worker([0,0,0],   0,  4,  4, faults[0])
	find_faulty_worker([0,0,0],   5,  9,  4, faults[1])
	find_faulty_worker([0,0,0,0], 10, 14, 4, faults[2])
	assert faults == [[1,3],[7,9],[10]]


def test_groupwise_splits():
	assert groupwise_splits(21/4+1, 21, 4, [1, 3, 5, 7, 9]) == [0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1]
	#assert groupwise_splits(5, 21, 4, [1, 2, 4, 7, 10, 20]) == [0, x, x, 0, x, 1, 1, x, 0, 0, x, 0, 1, 1, 1, 1, 0, 0, 0, 0]



if __name__ == '__main__':
	print divide_groups(21,5)
	test_find_faulty_worker()
	test_groupwise_splits()

