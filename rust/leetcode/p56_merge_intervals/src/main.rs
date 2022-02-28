/*
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
*/


/*
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
*/


struct Solution {}

impl Solution {
	pub fn merge(intervals: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
		use core::cmp::max;
		let is_overlapping = |x: &Vec<i32>, y: &Vec<i32>| y[0] <= x[1];
        // Merge two overlapping intervals and return the merged interval
        let merge_intervals = |x: &Vec<i32>, y: &Vec<i32>| vec![x[0], max(x[1], y[1])];
        
		let mut sorted_intervals = intervals.clone().to_owned();
		sorted_intervals.sort_by_key( |k| k[0] );
        let mut merged_intervals = vec![sorted_intervals[0].to_owned()];
		sorted_intervals.iter()
						.for_each( |interval| {
							let last_merged_interval = merged_intervals.last().unwrap();
							let last_idx = merged_intervals.len() - 1;
							if is_overlapping(&last_merged_interval, &interval) {
								merged_intervals[last_idx] = merge_intervals(&last_merged_interval, &interval);
							} else {
								merged_intervals.push(interval.to_vec());
							}
						});
		merged_intervals
	}
}


fn main() {
	println!("Run program!");
	assert!( Solution::merge(vec![vec![1,4], vec![1,2]]) == vec![vec![1,4]] );
}

#[cfg(test)]
mod basic_tests {
	use super::*;

    #[test]
    fn basic_tests() {
        assert_eq!(Solution::merge(vec![vec![1,3], vec![2,6], vec![8,10], vec![15,18]]), vec![vec![1,6], vec![8,10], vec![15,18]]);
		assert_eq!(Solution::merge(vec![vec![15,18], vec![1,3], vec![2,6], vec![8,10]]), vec![vec![1,6], vec![8,10], vec![15,18]]);
		assert_eq!(Solution::merge(vec![vec![1,4], vec![4,5]]), vec![vec![1,5]]);
		assert_eq!(Solution::merge(vec![vec![1,2], vec![3,4]]), vec![vec![1,2], vec![3,4]]);
		assert_eq!(Solution::merge(vec![vec![1,4], vec![0,4]]), vec![vec![0,4]]);
    }
}
