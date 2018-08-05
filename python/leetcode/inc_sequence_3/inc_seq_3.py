'''
https://leetcode.com/problems/increasing-triplet-subsequence/description/

 Given an unsorted array return whether an increasing subsequence of length 3 exists or not in the array.

Formally the function should:

    Return true if there exists i, j, k
    such that arr[i] < arr[j] < arr[k] given 0 <= i < j < k <= n-1 else return false. 

Your algorithm should run in O(n) time complexity and O(1) space complexity. 
'''

class Solution(object):
    def increasingTriplet(self, nums):
        """
        :type nums: List[int]
        :rtype: bool
        """

        #print 'The List: ', nums

        if len(nums) < 3:
            return False

        a = a_ = nums[0]
        b = c = None
        for x in nums[1:]:
            #print 'Processing x:', x
            #print 'current state (a,b,a_) is: ', a,b,a_

            # Found an a' that is lesser than current a
            # record this for now, and only replace a with a'
            # if b is not found yet, 
            #   in which case replace a with a' and return
            # *or* b has been found _and_ current element x: a < x < b,
            #   in this case, replace (a,b) with (a', x) because a'<a and a < x < b
            if x < a_:
                a_ = x
                #print 'Case 1: x < a_', x, '<', a_, '(a,a_):', a, a_
                continue

            if b is None:
                # Unconditionally update a with a_
                # Think, if the list starts with a decreasing subsequence, eg, 10, 9, 8, ...
                # a keeps getting updated to 10, then 9, and then 8, ...

                # if the list is not decreasing, a_ wouldn't have changed and would be = a
                #print 'b is None, updated a to a_', a, a_
                a = a_ 

                # We found a potential b
                if x > a:
                    #print 'b is None, found potential b, updated b to', x
                    b = x
            else: # b has been found
                # Check if we have found a 'c'
                # but only if we have already found 'a' and 'b'
                #print 'Case Pair (a,b) found so far'
                if x > b:
                    c = x
                    #print 'Found a triplet: ', a, b, c
                    return True
                elif (a_ < x < b): # Found a potential replacement for b
                    b = x
                    a = a_ # update a to a' if an a' < a was found earlier
                    #print 'Case (a,b), found potential b, updated (a,b) to', a, b

        #print 'Couldnt find a triplet, State (a,b,c,a_) :', a, b, c, a_
        return False




if __name__ == "__main__":
     myobj = Solution()
     assert( myobj.increasingTriplet([5,4]) is False)
     assert( myobj.increasingTriplet([1,2,3,4,5]) is True)
     assert( myobj.increasingTriplet([5,4,3,2,1]) is False)
     assert( myobj.increasingTriplet([1,4,3,5,2]) is True)
     assert( myobj.increasingTriplet([1,5,4,3,2]) is False)
     assert( myobj.increasingTriplet([9,2,7,1,3]) is False)
     assert( myobj.increasingTriplet([9,2,7,1,3,5]) is True)
     assert( myobj.increasingTriplet([1,1,1,1,1,1]) is False)
     assert( myobj.increasingTriplet([1,2,-10,-8,-7]) is True)
     assert( myobj.increasingTriplet([8,9,5,6,7]) is True)
     assert( myobj.increasingTriplet([1,2,1,2,1,2,1,2,1,2]) is False)
        
