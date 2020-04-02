'''
https://www.interviewbit.com/problems/merge-overlapping-intervals/

Merge Overlapping Intervals

Given a collection of intervals, merge all overlapping intervals.

For example:
	Given [1,3],[2,6],[8,10],[15,18],
	return [1,6],[8,10],[15,18].

	Make sure the returned intervals are sorted.
'''


'''
Solution Outline: O(nlogn) time, in-place
	Given two intervals, left, and right (left.start <= right.start, i.e. sorted by start time)
	 They can be merged if left.end <= right.start
	 otherwise they are disjoint

	 If the two intervals can be merged, merge right into left using
	   left.end = max(left.end, right.end), and remove right from the list
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
	def merge_overlapping_intervals(self, intervals):
		# sort intervals by interval start times
		intervals.sort(key=lambda x: x.start)

		i = 0
		while i+1 < len(intervals):
			left = intervals[i]
			right = intervals[i+1]
			# left's end interval is higher than next interval's start
			# the two intervals are disjoint
			# and cannot be merged
			if left.end < right.start:
				i += 1
			else:
				# left.end >= right.start
				intervals[i].end = max(left.end, right.end)
				intervals.pop(i+1)

		return intervals



def fromList(lst):
	return [Interval(a,b) for [a,b] in lst]

def toList(interval_lst):
	return [[interval.start, interval.end] for interval in interval_lst]


if __name__ == '__main__':
	s = Solution()
	assert s.merge_overlapping_intervals([]) == []
	assert s.merge_overlapping_intervals(fromList([[1,3]])) == fromList([[1,3]])
	assert s.merge_overlapping_intervals(fromList([[1,3],[2,6],[8,10],[15,18]])) == fromList([[1,6], [8,10], [15,18]])

