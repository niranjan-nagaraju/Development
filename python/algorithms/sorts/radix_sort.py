'''
Radix-sort:
    - Sort input numbers by their last-digits, then by their second-last all the way till their first-digits.
    - A naive approach uses a bucket per digit [0-9] and rearranges them in order to sort by digit,i. 

Sample run:
    a: [102, 091, 319, 910, 827, 888, 747, 589, 125, 420, 069, 127, 213, 624]
         0    1    2    3    4    5    6    7    8    9    10   11   12   13


Sort by last digit:
    bucket:
    [0] 910, 420,
    [1] 091,
    [2] 102, 
    [3] 213,
    [4] 624,
    [5] 125,
    [6]
    [7] 827, 747, 127,
    [8] 888,
    [9] 319, 589, 069,

    a: [910, 420, 091, 102, 213, 624, 125, 827, 747, 127, 888, 319, 589, 069]

Sort by 2nd-last digit:
    bucket:
    [0] 102,
    [1] 910, 213, 319,
    [2] 420, 624, 125, 827, 127,
    [3]
    [4] 747,
    [5] 
    [6] 069,
    [7]
    [8] 888, 589,
    [9] 091,

    a: [102, 910, 213, 319, 420, 624, 125, 827, 127, 747, 069, 888, 589, 091]

Sort by first digit:
    bucket:
    [0] 069, 091,
    [1] 102, 125, 127,
    [2] 213,
    [3] 319,
    [4] 420,
    [5] 589,
    [6] 624,
    [7] 747,
    [8] 827, 888,
    [9] 910,

    a: [069, 091, 102, 125, 127, 213, 319, 420, 589, 624, 747, 827, 888, 910]
'''


from data_structures.sll.queue import Queue
from math import log10, floor
def radix_sort_naive(nums):
	buckets = [Queue() for _ in xrange(10)]

	# calculate max number of digits for any number in list
	m = max(nums)
	num_digits = int(floor(log10(m))+1)

	denominator = 1
	for i in xrange(num_digits):
		# Separate numbers in list
		# into buckets based on their ith digit from the right
		for x in nums:
			digit = (x / denominator) % 10
			buckets[digit].enqueue(x)
		denominator *= 10

		# Replace original list
		# with numbers extracted from buckets[0-9]
		# in-order.
		j = 0
		for d in xrange(10):
			q = buckets[d]
			while q:
				nums[j] = q.dequeue()
				j += 1

	return nums

    
if __name__ == '__main__':
	a = [102, 91, 319, 910, 827, 888, 747, 589, 125, 420, 69, 127, 213, 624]
	sorted_a = sorted(a)
	assert radix_sort_naive(a) == sorted(a)
