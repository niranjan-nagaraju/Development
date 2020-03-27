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
Solution Outline: Brute Force O(n) time
	0. A: sorted array of intervals, (x,y): interval to be inserted
	1. Scan the array, A, Left-Right, until we find (a,b) s.t x > a
	   At this point,
	     . . . (a,b), (x,y), (i,j), ...
		 a < x, x < i
	2. If y is between (a,b) no need to add (x,y), however reduce (a,b),..., till we can no longer merge intervals
	3. Otherwise, 
	    Change (a,b) to (a, max(b, y)), then reduce from here.
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
		# If the interval to be inserted
		# has its start and end reversed
		# put them back in the right order
		if new_interval.start < new_interval.end:
			x,y = new_interval.start, new_interval.end
		else:
			x,y = new_interval.end, new_interval.start

		i = 0
		# Find a position to insert new interval
		for _ in intervals:
			a, b = _.start, _.end
			if x < a:
				break
			i += 1

		intervals.insert(i, Interval(x,y))

		j = i-1 if i > 0 else i
		while j+1 < len(intervals):
			a,b = intervals[j].start, intervals[j].end
			c,d = intervals[j+1].start, intervals[j+1].end

			# (a,b) and (c,d) cannot be merged because they are disjoint
			# skip this
			if b < c:
				j += 1
				continue

			# Stop when the next interval's start > new interval's end
			if c > y:
				break

			intervals[j] = Interval(a, max(b, d))
			intervals.pop(j+1)

		return intervals


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
	
