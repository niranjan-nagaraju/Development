/**
https://leetcode.com/problems/find-median-from-data-stream/

295. Find Median from Data Stream

Median is the middle value in an ordered integer list. If the size of the list is even, there is no middle value. So the median is the mean of the two middle value.

For example,
[2,3,4], the median is 3

[2,3], the median is (2 + 3) / 2 = 2.5

Design a data structure that supports the following two operations:

void addNum(int num) - Add a integer number from the data stream to the data structure.
double findMedian() - Return the median of all elements so far.
 
Example:
addNum(1)
addNum(2)
findMedian() -> 1.5
addNum(3) 
findMedian() -> 2

Follow up:
If all integer numbers from the stream are between 0 and 100, how would you optimize it?
If 99% of all integer numbers from the stream are between 0 and 100, how would you optimize it?
*/



/**
 Solution Outline:
	1. Use two heaps, one-maxheap and another minheap
	2. Max-heap 'left' contains the lower-half of the current items,
		Min-heap 'right' contains the upper-half of the current items
	3. 'left' and 'right' are balanced after reading each item in the stream.
		'left' can only utmost be 'right' +1 in size.
	4. Median == {left.top() + right.top()}/2 if left == right in size
		{left.top()} if left size == right size + 1
 */
#include <queue>
#include <vector>
#include <cassert>

class MedianFinder {
	std::priority_queue<int> left;
	std::priority_queue<int, std::vector<int>, std::greater<int>> right;

	public:
	/** initialize your data structure here. */
	MedianFinder() {

	}

	void addNum(int num) {
		if (!left.size()) {
			left.push(num);
			return;
		}

		if (num > left.top()) {
			right.push(num);
			if (right.size() > left.size()) {
				int x = right.top();
				right.pop();
				left.push(x);
			}
		} else {
			left.push(num);
			if (left.size() - right.size() > 1) {
				int x = left.top();
				left.pop();
				right.push(x);
			}
		}
	}

	double findMedian() {
		if (left.size() > right.size())
			return left.top();

		return (left.top() + right.top())/2.0;

	}
};


/**
 * Your MedianFinder object will be instantiated and called as such:
 * MedianFinder* obj = new MedianFinder();
 * obj->addNum(num);
 * double param_2 = obj->findMedian();
 */
int main(void)
{
	{
		MedianFinder m;

		m.addNum(41); // {41}
		assert(m.findMedian() == 41.0);

		m.addNum(35); // {35, 41}
		assert(m.findMedian() == 38.0);

		m.addNum(62); // {35, 41, 62}
		assert(m.findMedian() == 41.0);

		m.addNum(5); // {5, 35, 41, 62}
		assert(m.findMedian() == 38.0);

		m.addNum(97); // {5, 35, 41, 62, 97}
		assert(m.findMedian() == 41.0);

		m.addNum(108); // {5, 35, 41, 62, 97, 108}
		assert(m.findMedian() == 51.5);
	}

	{
		MedianFinder m;

		m.addNum(-1); // {-1}
		assert(m.findMedian() == -1.0);

		m.addNum(-2); // {-2, -1}
		assert(m.findMedian() == -1.5);

		m.addNum(-3); // {-3, -2, -1}
		assert(m.findMedian() == -2.0);

		m.addNum(-4); // {-4, -3, -2, -1}
		assert(m.findMedian() == -2.5);

		m.addNum(-5); // {-5, -4, -3, -2, -1}
		assert(m.findMedian() == -3.0);
	}
	return 0;
}

