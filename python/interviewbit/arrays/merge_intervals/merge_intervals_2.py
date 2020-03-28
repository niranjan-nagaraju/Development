#encoding: utf-8
'''
https://www.interviewbit.com/problems/merge-intervals/

Merge Intervals

Given a set of non-overlapping intervals, insert a new interval into the intervals (merge if necessary).

You may assume that the intervals were initially sorted according to their start times.

Example 1:
	Given intervals [1,3],[6,9] insert and merge [2,5] would result in [1,5],[6,9].

Example 2:
	Given [1,2],[3,5],[6,7],[8,10],[12,16], insert and merge [4,9] would result in [1,2],[3,10],[12,16].
	This is because the new interval [4,9] overlaps with [3,5],[6,7],[8,10].

Make sure the returned intervals are also sorted.
'''

'''
Solution Outline: Strictly O(n) time, O(n) space
   NOTE: Previous solution using in-place insert/remove in an array/list might be O(n²)
	0. A: sorted array of intervals, (x,y): interval to be inserted
	1. Use a list-based stack, 
	2. Scan the array, A, Left-Right, add them into the stack until we find (a,b) s.t x > a
	   At this point,
	     . . . (a,b), (x,y), (i,j), ...
		 a < x, x < i
	3. If y is between (a,b) no need to add (x,y), however reduce (a,b),..., till we can no longer merge intervals
	4. Otherwise, 
	    Change (a,b) to (a, max(b, y)), then reduce from here.
	5. Return the list/stack


Sample run:
	A: [1,2], [3,5], [6,7]. [8,10], [12,16]
	n: [4,9]

	Stack: []

	interval: [1,2]
	stack: [1,2]

	interval: [3,5]
	stack: [1,2], [3,5]

	interval: [6,7] > [4,9]
	stack top: [3,5] vs [4,9] -> merge === [3,9]
	stack: [1,2], [3,9]

	interval: [6,7]
	stack top: [3,9] vs [6,7] -> merge == [3,9]

	interval: [8,10]
	stack top: [3,9] vs [8,10] -> merge == [3,10]
	stack: [1,2], [3,10]

	interval: [12, 16], 12 > 9, copy everything as-is from here
	stack: [1,2], [3,10], [12,16]
'''

# Definition for an interval.
class Interval:
	def __init__(self, s=0, e=0):
		self.start = s
		self.end = e

	def __eq__(self, other):
		return self.start == other.start and self.end == other.end

	def __str__(self):
		return '(' + str(self.start) + ',' + str(self.end) + ')'


	def __repr__(self):
		return str(self)


class Solution:
    # @param intervals, a list of Intervals
    # @param new_interval, a Interval
    # @return a list of Interval
	def insert(self, intervals, new_interval):
		# helper function to
		# merge (a,b) and (c,d) in place into the stack
		# (a,b) is already in the stack top
		# NOTE: a < c since the intervals are sorted by start times
		def merge_intervals(interval):
			c, d = interval.start, interval.end

			if not stack:
				stack.append(interval)

			a,b = stack[-1].start, stack[-1].end
			if b < c:
				stack.append(Interval(c,d))
			stack[-1].end = max(b,d)


		# If the interval to be inserted
		# has its start and end reversed
		# put them back in the right order
		if new_interval.start > new_interval.end:
			new_interval.start, new_interval.end = new_interval.end, new_interval.start
		x,y = new_interval.start, new_interval.end

		# Copy intervals into the stack until
		# we find a position to insert new interval
		stack = []
		i = 0
		while i < len(intervals) and x >= intervals[i].start:
			stack.append(intervals[i])
			i += 1

		# all range starts < x are already in the stack
		# Merge (x,y) into the stack
		merge_intervals(new_interval)
			
		# Keep merging until the next interval's start > new interval's end
		while i < len(intervals) and intervals[i].start <= y:
			c,d = intervals[i].start, intervals[i].end

			merge_intervals(intervals[i])	
			i += 1

		# Done merging intervals until the current interval's start > new interval's end
		# copy the rest as-is
		while i < len(intervals):
			stack.append(intervals[i])
			i += 1

		return stack


# Convenience functions to convert a list of intervals to list of tuples and back
def fromList(lst):
	return [Interval(a,b) for [a,b] in lst]

def toList(interval_lst):
	return [[interval.start, interval.end] for interval in interval_lst]


if __name__ == '__main__':
	s = Solution()
	assert s.insert(fromList([(1,3), (6,9)]), Interval(2,5)) == fromList([(1,5), (6,9)])
	assert s.insert(fromList([(1,2), (3,6)]), Interval(10, 8)) == fromList([(1,2), (3,6), (8,10)])
	assert s.insert(fromList([(1,2), (3,5), (6,7), (8,10), (12,16)]), Interval(4, 9)) == fromList([(1,2), (3,10), (12,16)])

	A = fromList([(31, 38), (54, 76), (87, 94)])
	B = Interval(43, 68)
	assert s.insert(A, B) == fromList([(31,38), (43, 76), (87, 94)])
	
