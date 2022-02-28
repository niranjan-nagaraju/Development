/*
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
*/


/**
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

*/

struct Solution {}

impl Solution {
	pub fn remove_covered_intervals(intervals: Vec<Vec<i32>>) -> i32 {
		let mut sorted_intervals = intervals.clone().to_owned();

		// reverse-sort by ri followed by an ascending sort by li.
		// This ensures [1,2], [1,8] => [1,8], [1,2], ...
		sorted_intervals.sort_by( |a, b| b[1].cmp(&a[1]) );
		sorted_intervals.sort_by_key( |k| k[0] );

		fn covered_intervals( discrete: &Vec<Vec<i32>>, interval: &Vec<i32> ) -> bool {
			discrete.iter()
					.any( |di| di[0] <= interval[0] && di[1] >= interval[1]  )
		}

		let mut discrete_intervals = vec![sorted_intervals[0].to_owned()];
		sorted_intervals.iter()
						.for_each( |x| if !covered_intervals(&discrete_intervals, x) { discrete_intervals.push(x.to_vec()); } );

		return discrete_intervals.len() as i32;
	}
}


fn main() {
	let intervals = vec![ vec![1,4], vec![1,8], vec![2,9], vec![1,6] ];
	assert!( Solution::remove_covered_intervals(intervals) == 2 );
}

#[cfg(test)]
mod basic_tests {
	use super::*;

    #[test]
    fn basic_tests() {
        assert_eq!(Solution::remove_covered_intervals(vec![ vec![1,4], vec![3,6], vec![2,8]]), 2);
		assert_eq!(Solution::remove_covered_intervals(vec![ vec![1,4], vec![3,6], vec![2,8]]), 2);
		assert_eq!(Solution::remove_covered_intervals(vec![ vec![1,8], vec![1,4], vec![1,6]]), 1);
		assert_eq!(Solution::remove_covered_intervals(vec![ vec![1,4], vec![2,3]]), 1);
		assert_eq!(Solution::remove_covered_intervals(vec![ vec![2,3], vec![1,4]]), 1);
		assert_eq!(Solution::remove_covered_intervals(vec![ vec![1,3], vec![2,4], vec![3,5]]), 3);
    }
}


