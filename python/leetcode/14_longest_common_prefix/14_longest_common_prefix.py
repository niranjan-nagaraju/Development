'''
https://leetcode.com/problems/longest-common-prefix/description/

14. Longest Common Prefix

Write a function to find the longest common prefix string amongst an array of strings.
If there is no common prefix, return an empty string "".

Example 1:
    Input: strs = ["flower","flow","flight"]
    Output: "fl"
    Example 2:

    Input: strs = ["dog","racecar","car"]
    Output: ""
    Explanation: There is no common prefix among the input strings.

Constraints:
    1 <= strs.length <= 200
    0 <= strs[i].length <= 200
    strs[i] consists of only lowercase English letters.
'''


'''
Solution Outline:
   1. Use a dictionary to count frequency of various prefixes
   2. For each word of length `n`,
      2.1 Add all `n` prefixes to the `frequencies` dictionary and increment the frequencies.
   3. Filter prefixes with a frequency count == number-of-words, prefix with the maximum-length wins

E.g.,

strs = ["flower","flow","flight"]

frequencies: {}
"flower":
    frequencies
       "f" : 1
       "fl": 1
       "flo": 1
       "flow": 1 
       "flowe": 1
       "flower": 1

"flow":
    frequencies
       "f" : 2
       "fl": 2
       "flo": 2
       "flow": 2 
       "flowe": 1
       "flower": 1

"flight":
    frequencies
       "f" : 3
       "fl": 3
       "flo": 2
       "flow": 2 
       "flowe": 1
       "flower": 1
       "fli": 1
       "flig": 1
       "fligh": 1
       "flight": 1


Narrow prefixes with frequencies == 3 (len(strs))
    frequencies
       "f" : 3
       "fl": 3

    Return "fl" since it's bigger.
'''

from collections import defaultdict

class Solution:
    def longestCommonPrefix(self, strs):
        if not strs:
            return ""

        prefixCounts = defaultdict(int)
        for s in strs:
            prefixes = [s[:_+1] for _ in range(len(s))]
            for prefix in prefixes:
                prefixCounts[prefix] += 1

        lcp = ""
        for prefix in prefixCounts:
            if prefixCounts[prefix] != len(strs):
                continue

            if len(prefix) > len(lcp):
                lcp = prefix

        return lcp

            
if __name__ == '__main__':
    s = Solution()
    assert s.longestCommonPrefix(["flower","flow","flight"]) == "fl"
    assert s.longestCommonPrefix(["dog","racecar","car"]) == ""
