'''
https://adventofcode.com/2020/day/1

--- Day 1: Report Repair ---

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

--- Part Two ---
The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
'''

import sys

class Day1:
	def __init__(self, lst):
		self.lst = lst
		self.lst.sort()

	# returns two numbers a,b in `lst[startIdx..]` s.t a+b==target
	# expects `lst` to be sorted.
	@staticmethod
	def find_pairs_for_target_sum(lst, target, startIdx=0):
		i,j = startIdx, len(lst)-1
		while i < j:
			if lst[i] + lst[j] == target:
				return lst[i], lst[j]
			if lst[i] + lst[j] < target:
				i += 1
			elif lst[i] + lst[j] > target:
				j -= 1

		return None, None


	# Find a triplet (a,b,c) | a+b+c == target
	# For each `a` in lst
	#  Find a pair(b,c) to the right of `a` in lst | a+b+c == target, b+c == target-a
	@staticmethod
	def find_triplets_for_target_sum(lst, target):
		for i in xrange(len(lst)):
			a = lst[i]
			b,c = Day1.find_pairs_for_target_sum(lst, target-a, i+1)
			if b is not None:
				return a,b,c

		return None, None, None


	# Calculate a*b, s.t a+b==2020
	# a,b, in lst
	# `lst` is expected to be sorted
	def day1_p1(self):
		a, b = self.find_pairs_for_target_sum(self.lst, 2020)
		if a is None:
			raise ValueError("Pairs with target sum not found in list")
		return a*b


	# Calculate a*b*c, s.t a+b+c==2020
	# a,b,c in lst
	# `lst` is expected to be sorted
	def day1_p2(self):
		a, b, c = self.find_triplets_for_target_sum(self.lst, 2020)
		if a is None:
			raise ValueError("Triplets with target sum not found in list")
		return a*b*c

	def day1_accounting(self):
		return self.day1_p1(), self.day1_p2()


if __name__ == '__main__':
	d = Day1([1721, 979, 366, 299, 675, 1456])
	assert d.day1_p1() == 514579
	assert d.day1_p2() == 241861950
	
	d2 = Day1([int(l) for l in sys.stdin])
	print d2.day1_accounting()

