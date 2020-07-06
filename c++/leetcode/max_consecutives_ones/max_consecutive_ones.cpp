/*
https://leetcode.com/problems/max-consecutive-ones/

485. Max Consecutive Ones
Given a binary array, find the maximum number of consecutive 1s in this array.

Example 1:
Input: [1,1,0,1,1,1]
Output: 3
Explanation: The first two digits or the last three digits are consecutive 1s.
    The maximum number of consecutive 1s is 3.

Note:
The input array will only contain 0 and 1.
The length of input array is a positive integer and will not exceed 10,000
*/

#include <assert.h>
#include <vector>

class Solution {
	public:
		int findMaxConsecutiveOnes(const std::vector<int>& nums) {
			int count = 0, max_count=0;

			for (int x : nums) {
				count = (x == 0) ? 0 : (count+1);
				max_count = std::max(max_count, count);
			}

			return max_count;
		}
};

int
main(void)
{
	Solution s;
	assert(s.findMaxConsecutiveOnes(std::vector<int>{1,1,0,1,1,1}) == 3);
	assert(s.findMaxConsecutiveOnes(std::vector<int>{1,0,1,1,0,1}) == 2);
	return 0;
}

