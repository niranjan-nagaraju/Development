'''
https://leetcode.com/problems/remove-covered-intervals/description/

1288. Remove Covered Intervals

Given an array intervals where intervals[i] = [li, ri] represent the interval [li, ri), remove all intervals that are covered by another interval in the list.
The interval [a, b) is covered by the interval [c, d) if and only if c <= a and b <= d.
Return the number of remaining intervals.

Example 1:
Input: intervals = [[1,4],[3,6],[2,8]]
Output: 2
Explanation: Interval [3,6] is covered by [2,8], therefore it is removed.

Example 2:
Input: intervals = [[1,4],[2,3]]
Output: 1

Constraints:
    1 <= intervals.length <= 1000
    intervals[i].length == 2
    0 <= li <= ri <= 10^5
    All the given intervals are unique.
'''


'''
Solution Outline:
	1. Sort the intervals ordered by 'li', and reverse-ordered by ri.
	2. Start with discrete-intervals: [l0, r0]
		2.1 if [a,b] covers [c,d] and [c,d] covers [e,f], [a,b] covers [e,f] as well.
			=> [c,d] can safely be ignored for all comparisons against intervals to its right.
		2.2 For each interval [li, ri] (i: 1..n-1),
			if [li, ri] is covered by any interval in discrete-intervals => ignore [li,ri]
			else, add [li, ri] to discrete-intervals
		2.3 The sorting allows comparisons to stop right at [lj, rj] where lj > li

Sample run 1:
 [[1,4], [3,6], [2,8]]
 sort:
 [[1,4], [2,8], [3,6]]

 Discrete-intervals: [1,4]

 [2,8] is not covered by [1,4] => add,
 Discrete-intervals: [1,4], [2,8]

 [3,6] is not covered by [1,4], 
 [3,6] is covered by [2,8] -> skip

 Discrete-intervals: [1,4], [2,8]
 return 2

 Sample run 2:
 [[1,4], [1,8]]
 sort:
 [[1,8], [1,4]]

 Discrete-intervals: [1,8]
 [1,4] is covered by [1,8] => skip
'''

from operator import itemgetter
class Solution:
	def removeCoveredIntervals(self, intervals):
		# sort intervals by li, then reverse-sorted by ri
		sorted_intervals = sorted(intervals, key=itemgetter(1), reverse=True) # first reverse-sort by ri
		sorted_intervals = sorted(sorted_intervals, key=itemgetter(0)) # then sort by li
		# check if (a,b) covers (c,d)
		covered_intervals = lambda (a,b), (c,d): c >= a and d <= b
		discrete_intervals = [sorted_intervals[0]]
		for interval in sorted_intervals[1:]:
			covered = False
			for di in discrete_intervals:
				if covered_intervals(di, interval):
					covered = True
					break
			if not covered:
				discrete_intervals.append(interval)

		return len(discrete_intervals)


if __name__ == '__main__':
	s = Solution()
	assert s.removeCoveredIntervals([[1,8],[1,4],[1,6]]) == 1
	assert s.removeCoveredIntervals([[1,4],[3,6],[2,8]]) == 2
	assert s.removeCoveredIntervals([[1,4],[2,3]]) == 1
	assert s.removeCoveredIntervals([[2,3],[1,4]]) == 1
	assert s.removeCoveredIntervals([[1,3],[2,4],[3,5]]) == 3

