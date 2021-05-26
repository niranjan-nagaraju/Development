from data_structures.sll.queue import Queue
from math import log10, floor


'''
Radix-sort (naive approach):
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



'''
Efficient implementation using counting sort/prefix-sum
Source: Creel/https://www.youtube.com/watch?v=ujb2CIWE8zY

Radix-sort(Efficient version):
    - Sort input numbers by their last-digits, then by their second-last all the way till their first-digits.
    - Use counting/prefix-sums to keep track of individual digit frequencies, and use it as a replacement for the queue.

Sample run:
      a: [102, 091, 319, 910, 827, 888, 747, 589, 125, 420, 069, 127, 213, 624]
          0    1    2    3    4    5    6    7    8    9    10   11   12   13


Sort by last digit:
    prefix: [Increment respective bucket frequency by last digit]
      [ 2    1    1    1    1    1    0    3    1    3 ]
        0    1    2    3    4    5    6    7    8    9

    calculate cumulative prefix-sums:
      [ 2    3    4    5    6    7    7    10   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    Use cumulative prefix-sums to sort numbers into their buckets based on last digit.
    b: [---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    R-L, 
    a[-1]: 624, index: 4, cumulative-prefix-sums[4]: 6 => store 624 at index 5 in temporary array, b[]
    decrement cumulative-prefix-sums[4] by 1
    b[5] = 624
    b: [---, ---, ---, ---, ---, 624, ---, ---, ---, ---, ---, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 2    3    4    5    5    7    7    10   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-2]: 213, cumulative-prefix-sums[3]: 5 => b[4] = 213
    b: [---, ---, ---, ---, 213, 624, ---, ---, ---, ---, ---, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 2    3    4    4    5    7    7    10   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-3]: 127, cumulative-prefix-sums[7]: 10 => b[9] = 127
    b: [---, ---, ---, ---, 213, 624, ---, ---, ---, 127, ---, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 2    3    4    5    4    7    7    9   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-4]: 069, cumulative-prefix-sums[9]: 14 => b[13] = 069
    b: [---, ---, ---, ---, 213, 624, ---, ---, ---, 127, ---, ---, ---, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 2    3    4    5    4    7    7    9   11   13 ]
        0    1    2    3    4    5    6    7    8    9

    a[-5]: 420, cumulative-prefix-sums[0]: 2 => b[1] = 420
    b: [---, 420, ---, ---, 213, 624, ---, ---, ---, 127, ---, ---, ---, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    7    7    9   11   13 ]
        0    1    2    3    4    5    6    7    8    9

    a[-6]: 125, cumulative-prefix-sums[5]: 7 => b[6] = 125
    b: [---, 420, ---, ---, 213, 624, 125, ---, ---, 127, ---, ---, ---, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    6    7    9   11   13 ]
        0    1    2    3    4    5    6    7    8    9

    a[-7]: 589, cumulative-prefix-sums[9]: 13 => b[12] = 589
    b: [---, 420, ---, ---, 213, 624, 125, ---, ---, 127, ---, ---, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    6    7    9   11   12 ]
        0    1    2    3    4    5    6    7    8    9

    a[-8]: 747, cumulative-prefix-sums[7]: 9 => b[8] = 747
    b: [---, 420, ---, ---, 213, 624, 125, ---, 747, 127, ---, ---, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    6    7    8   11   12 ]
        0    1    2    3    4    5    6    7    8    9

    a[-9]: 888, cumulative-prefix-sums[8]: 11 => b[10] = 888
    b: [---, 420, ---, ---, 213, 624, 125, ---, 747, 127, 888, ---, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    6    7    8   10   12 ]
        0    1    2    3    4    5    6    7    8    9

    a[-10]: 827, cumulative-prefix-sums[7]: 8 => b[7] = 827
    b: [---, 420, ---, ---, 213, 624, 125, 827, 747, 127, 888, ---, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    4    5    4    6    7    7   10   12 ]
        0    1    2    3    4    5    6    7    8    9

    a[-11]: 910, cumulative-prefix-sums[0]: 1 => b[0] = 910
    b: [910, 420, ---, ---, 213, 624, 125, 827, 747, 127, 888, ---, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    3    4    5    4    6    7    7   10   12 ]
        0    1    2    3    4    5    6    7    8    9

    a[-12]: 319, cumulative-prefix-sums[9]: 12 => b[11] = 319
    b: [910, 420, ---, ---, 213, 624, 125, 827, 747, 127, 888, 319, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    3    4    5    4    6    7    7   10   11 ]
        0    1    2    3    4    5    6    7    8    9

    a[-13]: 091, cumulative-prefix-sums[1]: 3 => b[2] = 091
    b: [910, 420, 091, ---, 213, 624, 125, 827, 747, 127, 888, 319, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    2    4    5    4    6    7    7   10   11 ]
        0    1    2    3    4    5    6    7    8    9

    a[-14]: 102, cumulative-prefix-sums[2]: 4 => b[3] = 102
    b: [910, 420, 091, 102, 213, 624, 125, 827, 747, 127, 888, 319, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    2    4    5    4    6    7    7   10   11 ]
        0    1    2    3    4    5    6    7    8    9


Sort by 2nd digit:
    a: [910, 420, 091, 102, 213, 624, 125, 827, 747, 127, 888, 319, 589, 069]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    prefix: [Increment respective bucket frequency by 2nd digit]
      [ 1    3    5    0    1    0    1    0    2    1 ]
        0    1    2    3    4    5    6    7    8    9

    calculate cumulative prefix-sums:
      [ 1    4    9    9    10   10   11   11   13   14 ]
        0    1    2    3    4    5    6    7    8    9

    Use cumulative prefix-sums to sort numbers into their buckets based on last digit.
    b: [---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    R-L, 
    a[-1]: 069, index: 6, cumulative-prefix-sums[6]: 11 => store 069 at index 10 in temporary array, b[]
    decrement cumulative-prefix-sums[9] by 1
    b[10] = 069
    b: [---, ---, ---, ---, ---, ---, ---, ---, ---, ---, 069, ---, ---, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    4    9    9    10   10   10   11   13   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-2]: 589, cumulative-prefix-sums[8]: 13 => b[12] = 589
    b: [---, ---, ---, ---, ---, ---, ---, ---, ---, ---, 069, ---, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    4    9    9    10   10   10   11   12   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-3]: 319, cumulative-prefix-sums[1]: 4 => b[3] = 319
    b: [---, ---, ---, 319, ---, ---, ---, ---, ---, ---, 069, ---, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    9    9    10   10   10   11   12   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-4]: 888, cumulative-prefix-sums[8]: 12 => b[11] = 888
    b: [---, ---, ---, 319, ---, ---, ---, ---, ---, ---, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    9    9    10   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-5]: 127, cumulative-prefix-sums[2]: 9 => b[8] = 127
    b: [---, ---, ---, 319, ---, ---, ---, ---, 127, ---, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    8    9    10   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-6]: 747, cumulative-prefix-sums[4]: 10 => b[9] = 747
    b: [---, ---, ---, 319, ---, ---, ---, ---, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    8    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-7]: 827, cumulative-prefix-sums[2]: 8 => b[7] = 827
    b: [---, ---, ---, 319, ---, ---, ---, 827, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    7    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-8]: 125, cumulative-prefix-sums[2]: 7 => b[6] = 125
    b: [---, ---, ---, 319, ---, ---, 125, 827, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    6    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-9]: 624, cumulative-prefix-sums[2]: 6 => b[5] = 624
    b: [---, ---, ---, 319, ---, 624, 125, 827, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    3    5    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-10]: 213, cumulative-prefix-sums[1]: 3 => b[2] = 213
    b: [---, ---, 213, 319, ---, 624, 125, 827, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 1    2    5    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-11]: 102, cumulative-prefix-sums[0]: 1 => b[1] = 102
    b: [102, ---, 213, 319, ---, 624, 125, 827, 127, 747, 069, 888, 589, ---]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    2    5    9    9   10   10   11   11   14 ]
        0    1    2    3    4    5    6    7    8    9

    a[-12]: 091, cumulative-prefix-sums[9]: 14 => b[13] = 091
    b: [102, ---, 213, 319, ---, 624, 125, 827, 127, 747, 069, 888, 589, 091]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    2    5    9    9   10   10   11   11   13 ]
        0    1    2    3    4    5    6    7    8    9

    a[-13]: 420, cumulative-prefix-sums[2]: 5 => b[4] = 420
    b: [102, ---, 213, 319, 420, 624, 125, 827, 127, 747, 069, 888, 589, 091]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    2    4    9    9   10   10   11   11   13 ]
        0    1    2    3    4    5    6    7    8    9

    a[-14]: 910, cumulative-prefix-sums[1]: 2 => b[1] = 910
    b: [102, 910, 213, 319, 420, 624, 125, 827, 127, 747, 069, 888, 589, 091]
        0    1    2    3    4    5    6    7    8    9    10   11   12   13
    cumulative-prefix-sums:
      [ 0    1    4    9    9   10   10   11   11   13 ]
        0    1    2    3    4    5    6    7    8    9
'''

if __name__ == '__main__':
	a = [102, 91, 319, 910, 827, 888, 747, 589, 125, 420, 69, 127, 213, 624]
	sorted_a = sorted(a)
	assert radix_sort_naive(a) == sorted(a)
