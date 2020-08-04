'''
https://www.interviewbit.com/problems/majority-element/

Majority Element

Given an array of size n, find the majority element. The majority element is the element that appears more than floor(n/2) times.

You may assume that the array is non-empty and the majority element always exist in the array.

Example :
	Input : [2, 1, 2]
	Return  : 2 which occurs 2 times which is greater than 3/2. 
'''


'''
Solution Outline:
    0. Use Moore's voting algorithm
    1. Start  with A[0] as candidate, count: 1
        1.1 For each A[i] check if A[i] == candidate, if yes, increment count
                otherwise, decrement count, and if count reaches 0, Set A[i] as candidate with count as 1
        1.2 At the end of the pass, if count is non-zero (we *might* have a majority element)
        1.3 Check its frequency to confirm in a second-pass.


Sample run:
    A: [1, 2, 3, 2, 1, 2, 2]

    i: 0, A[i]: 1
    candidate: 1
    count: 1

    i: 1, A[i]: 2 != candidate
    count = 0
    candidate: 2
    count: 1

    i: 2, A[i]: 3 != candidate
    count = 0
    candidate: 3
    count: 1

    i: 3, A[i]: 2 != candidate
    count = 0
    candidate: 2
    count: 1

    i: 4, A[i]: 1 != candidate
    count = 0
    candidate: 1
    count: 1

    i: 5, A[i]: 2 != candidate
    count = 0
    candidate: 2
    count: 1

    i: 6, A[i]: 2 == candidate
    count = 2

    candidate 2, count = 2
    majority element *might* be 2

    freq[2] == 4 > 7/2 => majority element = 2
'''
class Solution:
    def find_majority_element(self, A):
        candidate = A[0]
        count = 1

        for x in A[1:]:
            if x == candidate:
                count += 1
            else:
                if count == 1:
                    candidate = x
                else:
                    count -= 1

        if count > 0:
            count = 0
        #else:
        #   No majority element in A
        #   return -1
        for x in A:
            if x == candidate:
                count += 1

        assert(count > len(A)/2)
        return candidate


if __name__ == '__main__':
    s = Solution()
    assert s.find_majority_element([2,1,2]) == 2
    assert s.find_majority_element([1,2,3,2,2]) == 2


