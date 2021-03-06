/**
https://leetcode.com/problems/maximum-xor-of-two-numbers-in-an-array/

421. Maximum XOR of Two Numbers in an Array

Given an integer array nums, return the maximum result of nums[i] XOR nums[j], where 0 ≤ i ≤ j < n.

Follow up: Could you do this in O(n) runtime?

Example 1:
Input: nums = [3,10,5,25,2,8]
Output: 28
Explanation: The maximum result is 5 XOR 25 = 28.

Example 2:
Input: nums = [0]
Output: 0

Example 3:
Input: nums = [2,4]
Output: 6

Example 4:
Input: nums = [8,10,2]
Output: 10

Example 5:
Input: nums = [14,70,53,83,49,91,36,80,92,51,66,70]
Output: 127

Constraints:
1 <= nums.length <= 2 * 10^4
0 <= nums[i] <= 2^31 - 1
*/



/*
Solution Outline:
    0. Use a hash-table/dictionary to build a prefix trie for the array, B
    1. For each item, B[i], in B
       1.1 Traverse bits, MSB to LSB and for every bit b in B[i], match the opposite bit, b' in the trie
           to maximize b xor b' == 1 if b != b'
       1.2 If no such b' is found in the trie, traverse b in the path, and onto the next bit.
       1.3 Set or reset the bit b position in current xor if b' is found or not respectively.
    2. Return max of the current xors found for each B[i]

Sample run:
    B: [1, 2, 3] -> [001, 010, 011]
    B as a prefix tree
            *
          /
         0
      /    \
     0      1
      \    / \
       1  0   1

    Initially max xor: 0b000

    B[0] = 1 = 0b001
    B[0][0] = 0
    Locate '1' in trie level 1, so we get B[0][0] ^ y == 1 (path: *1)
    '1' not found at level1, Pick 0 instead, current xor: 0, max xor: 0
    B[0][1] = 0
    Locate '1' in (level 1, 0) children, found '1' (path: *01)
        => set 2nd bit in current_xor: 0b010
    B[0][2] = 1
    Locate '0' in (level 2, 1) children, found '0' (path: *010)
        => Set 3rd bit in current_xor: 0b011 > max_xor
        max_xor = 0b011 using B[0] and any of B[]
        1 ^ 1 = 0b000
        1 ^ 2 = 0b011
        1 ^ 3 = 0b010, max: 0b011

    B[1] = 2 = 0b010
      B[1][0] = 0, locate *1 in trie. Not found, => pick '0' instead,
      current xor: 0
      B[1][1] = 1, locate *00 in trie, => found => set 2nd bit in current xor
      current xor: 0b010
      B[1][2] = 0, locate *001 in trie => found => set 3rd bit in current xor
      current xor: 0b011
      == max xor
      => using only B[1] = 2, and any of B[]
      max_xor = 0b011
      2 ^ 1 = 0b011
      2 ^ 2 = 0b000
      2 ^ 3 = 0b001, max: 0b011

    B[2] = 3 = 0b011
      B[2][0] = 0, locate *1 in trie, Not found => pick 0 instead
      current xor: 0b000
      B[2][1] = 1, locate *00 in trie, Not found => set 2nd bit in current xor
      current xor: 0b010
      B[2][2] = 1, locate *000 in trie, Not found =>
      current xor: 0b010
      > max_xor = 0b010
       => using only B[2] = 3, and any of B[]
       max_xor = 0b010
       3 ^ 1 = 0b010
       3 ^ 2 = 0b001
       3 ^ 3 = 0b000
    => max xor: 0b011 = 3
*/





mod bit_array;
mod binary_prefix_map;
use bit_array::BitArray;
use binary_prefix_map::BinaryPrefixSet;

struct Solution;

impl Solution {
    pub fn find_maximum_xor(nums: Vec<i32>) -> i32 {
		if nums.len() <= 1 {
			return 0;
		}

		let max_number = nums.iter().max().unwrap();
		let max_bit_width = ((*max_number as f64).log2().floor() as u32) + 1;

		// Build a binary (prefix) map out of B
		let mut bt = BinaryPrefixSet::new(max_bit_width);
		for x in &nums {
			bt.add(*x as u32);
		}

		nums.iter()
			.map( |x| bt.calculate_max_xor(*x as u32) )
			.max()
			.unwrap() as i32
    }
} 

fn main() {
	println!("Running tests");
	assert_eq!( Solution::find_maximum_xor(vec![1,2,3]), 0b011 );
	assert_eq!( Solution::find_maximum_xor(vec![1,2,4]), 0b110 );
	assert_eq!( Solution::find_maximum_xor(vec![3,10,5,25,2,8]), 28);
	assert_eq!( Solution::find_maximum_xor(vec![0]), 0 );
	assert_eq!( Solution::find_maximum_xor(vec![2,4]), 6 );
	assert_eq!( Solution::find_maximum_xor(vec![8,10,2]), 10 );
	assert_eq!( Solution::find_maximum_xor(vec![14,70,53,83,49,91,36,80,92,51,66,70]), 127 );
	println!(" Tests passed");
}


