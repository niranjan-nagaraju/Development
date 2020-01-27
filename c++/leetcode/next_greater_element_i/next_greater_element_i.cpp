/**
https://leetcode.com/problems/next-greater-element-i/

496. Next Greater Element I
You are given two arrays (without duplicates) nums1 and nums2 where nums1’s elements are subset of nums2. Find all the next greater numbers for nums1's elements in the corresponding places of nums2.

The Next Greater Number of a number x in nums1 is the first greater number to its right in nums2. If it does not exist, output -1 for this number.

Example 1:
Input: nums1 = [4,1,2], nums2 = [1,3,4,2].
Output: [-1,3,-1]
Explanation:
    For number 4 in the first array, you cannot find the next greater number for it in the second array, so output -1.
	For number 1 in the first array, the next greater number for it in the second array is 3.
	For number 2 in the first array, there is no next greater number for it in the second array, so output -1.

Example 2:
Input: nums1 = [2,4], nums2 = [1,2,3,4].
Output: [3,-1]
Explanation:
    For number 2 in the first array, the next greater number for it in the second array is 3.
	For number 4 in the first array, there is no next greater number for it in the second array, so output -1.

Note:
All elements in nums1 and nums2 are unique.
The length of both nums1 and nums2 would not exceed 1000.
*/

/**
 Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack
*/

#include <assert.h>
#include <vector>
#include <iostream>
#include <unordered_map>
#include "next_greater_element_i.hpp"
using namespace std;

class Stack {
	private:
		vector<int> *items;

	public:
		Stack() {
			this->items = new vector<int>();
		}

		void push(int x) {
			this->items->push_back(x);
		}

		int pop(void) {
			int x = this->top();
			this->items->pop_back();

			return x;
		}

		int top(void) {
			if (this->size() > 0)
				return this->items->back();
			
			return -1;
		}

		int size(void) {
			return this->items->size();
		}
};



vector<int> 
Solution::nextGreaterElement(vector<int>& nums1, vector<int>& nums2)
{
	vector<int> nge;
	Stack s;

	unordered_map<int, int>nge2;
	unordered_map<int, int>::const_iterator it;

	for (int curr : nums2) {
		while (s.size() && s.top() < curr) {
			int x = s.pop();

			// set all popped item's nge as current element
			nge2[x] = curr;
		}
		s.push(curr);
	}

	for (int x : nums1) {
		it = nge2.find(x);
		if (it == nge2.end())
			nge.push_back(-1);
		else
			nge.push_back(it->second);
	}

	return nge;
}

