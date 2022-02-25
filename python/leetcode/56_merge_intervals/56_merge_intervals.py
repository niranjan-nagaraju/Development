'''
https://leetcode.com/problems/merge-intervals/

56. Merge Intervals
Given an array of intervals where intervals[i] = [starti, endi], merge all overlapping intervals, and return an array of the non-overlapping intervals that cover all the intervals in the input.

Example 1:
Input: intervals = [[1,3],[2,6],[8,10],[15,18]]
Output: [[1,6],[8,10],[15,18]]
Explanation: Since intervals [1,3] and [2,6] overlaps, merge them into [1,6].

Example 2:
Input: intervals = [[1,4],[4,5]]
Output: [[1,5]]
Explanation: Intervals [1,4] and [4,5] are considered overlapping.

Constraints:
    1 <= intervals.length <= 10^4
    intervals[i].length == 2
    0 <= starti <= endi <= 10^4
'''


'''
Solution Outline:
    1. Sort the list by starti
    2. Current-interval -> (start0, end0)
       2.1 Merge consecutive intervals into current interval until the next interval does not overlap with the current(merged) one.
           2.1.1 Two consecutive intervals [a,b], [c,d] overlap if (c <= b) [NOTE: c >= a because the intervals are sorted]
                 Non-overlapping intervals cannot be merged.
           2.1.2 The intervals [a,b] amd [c,d] can be merged if overlapping :-
                 [x,y] = [a, max(b,d)]
                 e.g,
                 [1,4], [2,5] => [1,5]
       2.2 At this point, we have a current interval(merged consecutive intervals), next-interval: [starti, endi] which does not overlap with current-interval.
           Add current-interval to results.
           Restart 2. with current-interval = [starti, endi]
	3. Add last-interval = [starti, endi] if it doesn't overlap with previous current-interval.

Sample run:
  [ [1,3], [2,6]], [8,10], [15,18] ]
  Merged-intervals: []

  current: [1,3]
  s,e: [2,6] -> overlaps with current => merge
  current: [1,6]

  s,e: [8,10] -> no overlap
  Merged-intervals: [1,6]
  current: [8,10]

  s,e: [15,18] -> no overlap
  Merged-intervals: [1,6], [8,10]
  current: [15,18]

  Merged-intervals: [1,6], [8,10], [15,18]
'''

class Solution:
    def merge(self, intervals):
        isOverlapping = lambda (a,b), (c,d): c <= b
        # Merge two overlapping intervals and return the merged interval
        mergeIntervals = lambda (a,b), (c,d): [a, max(b,d)]
        intervals.sort()
        merged_intervals = [intervals[0]]
        for interval in intervals:
            if isOverlapping(merged_intervals[-1], interval):
                merged_intervals[-1] = mergeIntervals(merged_intervals[-1], interval)
            else:
                merged_intervals.append(interval)
            
        return merged_intervals


if __name__ == '__main__':
    s = Solution()
    assert s.merge([[1,3],[2,6],[8,10],[15,18]]) == [[1,6],[8,10],[15,18]]
    assert s.merge([[15,18], [1,3],[2,6],[8,10]]) == [[1,6],[8,10],[15,18]]
    assert s.merge([[1,4],[4,5]]) == [[1,5]]
    assert s.merge([[1,2],[3,4]]) == [[1,2],[3,4]]
    assert s.merge([[1,4],[0,4]]) == [[0,4]]

